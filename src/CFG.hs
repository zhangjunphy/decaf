-- CFG -- Control Flow Graph with SSA nodes
-- Copyright (C) 2018 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
module CFG where

import AST qualified
import Control.Applicative ((<|>))
import Control.Exception (throw)
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Generics.Labels
import Data.GraphViz.Types.Graph qualified as GV
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic (lookupLocalMethodFromST)
import Semantic qualified as SE
import Types
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL

data Condition
  = Pred {pred :: VarOrImm}
  | Complement
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: ScopeID,
    statements :: [SSA]
  }
  deriving (Generic, Show)

data CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge

data SymVarMap = SymVarMap
  { m :: Map Name VID,
    parent :: Maybe ScopeID
  }
  deriving (Show, Generic)

data CFGState = CFGState
  { cfg :: CFG,
    astScope :: ScopeID,
    sig :: AST.MethodSig,
    currentBBID :: BBID,
    vars :: VarList,
    sym2var :: Map ScopeID SymVarMap,
    var2sym :: Map VID (ScopeID, Name),
    statements :: [SSA],
    currentControlBlock :: Maybe (BBID, BBID), -- entry and exit
    currentFunctionTail :: Maybe BBID
  }
  deriving (Generic)

data BBTransition
  = StayIn !BBID
  | TailAt !BBID
  | Deadend

initialState :: AST.MethodSig -> CFGState
initialState sig =
  CFGState
    G.empty
    0
    sig
    0
    []
    (Map.fromList [(0, SymVarMap Map.empty Nothing)])
    Map.empty
    []
    Nothing
    Nothing

-- Helps for CFGBuild monad
updateCFG :: G.GraphBuilder BBID CFGNode CFGEdge a -> CFGBuild ()
updateCFG update = do
  g <- use #cfg
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CFGExcept m
    Right g -> #cfg .= g

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SE.SymbolTable
  }
  deriving (Generic)

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGBuild a = CFGBuild
  { runCFGBuild ::
      ExceptT
        CFGExcept
        (ReaderT CFGContext (State CFGState))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError CFGExcept,
      MonadReader CFGContext,
      MonadState CFGState
    )

setASTScope :: ScopeID -> CFGBuild ()
setASTScope sid = do
  #astScope .= sid

{-----------------------------------------------------------
  Record current innermost control block (while/for/etc.)
  for continue/break to find correct successor block.
------------------------------------------------------------}
setControlBlock :: Maybe (BBID, BBID) -> CFGBuild ()
setControlBlock entryAndExit = do
  #currentControlBlock .= entryAndExit

getControlBlock :: CFGBuild (Maybe (BBID, BBID))
getControlBlock = use #currentControlBlock

getControlEntry :: CFGBuild (Maybe BBID)
getControlEntry = use #currentControlBlock <&> (<&> (^. _1))

getControlExit :: CFGBuild (Maybe BBID)
getControlExit = use #currentControlBlock <&> (<&> (^. _2))

{-----------------------------------------------------------
  Record function tail for return to find correct
  successor block.
------------------------------------------------------------}
setFunctionTail :: Maybe BBID -> CFGBuild ()
setFunctionTail tail = do
  #currentFunctionTail .= tail

getFunctionTail :: CFGBuild (Maybe BBID)
getFunctionTail = use #currentFunctionTail

addVarSym :: Name -> VID -> CFGBuild ()
addVarSym name vid = do
  sid <- use #astScope
  -- update var->sym
  #var2sym %= Map.insert vid (sid, name)
  -- update sym->var
  sym2var <- use #sym2var
  let sym2varInScope = Map.lookup sid sym2var
  case sym2varInScope of
    Nothing -> throwError $ CFGExcept "Unable to find scope in sym->var map"
    Just s2v -> #sym2var %= Map.insert sid (s2v & #m %~ Map.insert name vid)

lookupVar :: VID -> CFGBuild (Maybe (ScopeID, Name))
lookupVar vid = uses #var2sym $ Map.lookup vid

-- Look for symbol resursively
lookupSym :: Name -> CFGBuild (Maybe VID)
lookupSym name = do
  sid <- use #astScope
  sym2var <- use #sym2var
  return $ Map.lookup sid sym2var >>= (\m' -> lookup name m' sym2var)
  where
    lookup :: Name -> SymVarMap -> Map ScopeID SymVarMap -> Maybe VID
    lookup name symVarMap sym2var = case Map.lookup name (symVarMap ^. #m) of
      Just vid -> Just vid
      Nothing ->
        (symVarMap ^. #parent)
          >>= (`Map.lookup` sym2var)
          >>= \map' -> lookup name map' sym2var

getVarDecl :: Name -> CFGBuild (Maybe (Either AST.Argument AST.FieldDecl))
getVarDecl name = do
  sid <- use #astScope
  sts <- view #symbolTables
  sig <- use #sig
  case Map.lookup sid sts of
    Nothing -> throwError $ CFGExcept (sformat ("Unable to find scope " % int) sid)
    Just st -> return $ lookup name st
  where
    lookup name st' =
      (SE.lookupLocalVariableFromST name st')
        <|> (SE.parent st' >>= lookup name)

createEmptyBB :: CFGBuild BBID
createEmptyBB = do
  checkStmts
  #currentBBID += 1
  bbid <- use #currentBBID
  sid <- use #astScope
  let bb = BasicBlock bbid sid []
  updateCFG (G.addNode bbid (CFGNode bb))
  return bbid

finishCurrentBB :: CFGBuild BBID
finishCurrentBB = do
  bbid <- use #currentBBID
  stmts <- use #statements
  sid <- use #astScope
  #statements .= []
  let bb = BasicBlock bbid sid stmts
  updateCFG (G.addNode bbid (CFGNode bb))
  #currentBBID += 1
  return bbid

checkStmts :: CFGBuild ()
checkStmts = do
  stmts <- use #statements
  unless (null stmts) $ throwError $ CFGExcept $ Text.pack $ "Dangling statements found: " ++ show stmts

-- Remove empty seq node one by one
removeEmptySeqNode :: CFGBuild ()
removeEmptySeqNode = do
  g@G.Graph {nodes = nodes} <- gets cfg
  let emptySeqNode =
        List.find
          (\(ni, nd) -> isEmptyNode nd && isSeqOut ni g)
          $ Map.assocs nodes
  case emptySeqNode of
    Nothing -> return ()
    Just (ni, _) -> do
      updateCFG $
        let inEdges = G.inBound ni g
            outEdge = head $ G.outBound ni g
         in bridgeEdges ni inEdges outEdge
      removeEmptySeqNode
  where
    isEmptyNode CFGNode {bb = BasicBlock {statements = stmts}} = null stmts
    isSeqOut ni g =
      let outEdges = G.outBound ni g
       in length outEdges == 1 && isSeqEdge (snd $ head outEdges)
    bridgeEdges mid inEdges (out, _) =
      forM_
        inEdges
        ( \(ni, ed) -> do
            G.deleteNode mid
            G.addEdge ni out ed
        )
    isSeqEdge SeqEdge = True
    isSeqEdge _ = False

newVar :: Maybe Name -> AST.Type -> SL.Range -> CFGBuild Var
newVar Nothing tpe sl = do
  vars <- use #vars
  let vid = length vars
  let var = Var vid tpe Nothing sl
  #vars .= vars ++ [var]
  return var
newVar (Just name) tpe sl = do
  vars <- use #vars
  let vid = length vars
  decl <- getVarDecl name
  when (isNothing decl) $ throwError (CFGExcept $ sformat ("Unable to find decl of variable " % stext) name)
  let var = Var vid tpe decl sl
  #vars .= vars ++ [var]
  addVarSym name vid
  return var

addSSA :: SSA -> CFGBuild ()
addSSA ssa = do
  stmts <- use #statements
  #statements .= stmts ++ [ssa]

findVarOfSym :: Name -> CFGBuild (Maybe Var)
findVarOfSym name = do
  vars <- use #vars
  vid <- lookupSym name
  return $ vid <&> (vars !!)

findVarOfSym' :: Name -> CFGBuild Var
findVarOfSym' name = do
  var <- findVarOfSym name
  case var of
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable " % stext) name
    Just v -> return v

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
buildCFG root@(AST.ASTRoot _ _ methods) context =
  let build method =
        runState $
          flip runReaderT context $
            runExceptT $
              runCFGBuild (populateGlobals root >> buildMethod method)
      updateMap map m =
        let (r, _) = build m $ initialState (m ^. #sig)
         in case r of
              Left e -> Left e
              Right r' -> Right $ Map.insert (m ^. (#sig . #name)) r' map
   in foldM updateMap Map.empty methods

-- Build cfg from ast fragments

populateGlobals :: AST.ASTRoot -> CFGBuild ()
populateGlobals root@(AST.ASTRoot _ globals _) = do
  sid <- use #astScope
  setASTScope 0
  mapM_ (\(AST.FieldDecl name tpe loc) -> newVar (Just name) tpe loc) globals
  setASTScope sid

buildMethod :: AST.MethodDecl -> CFGBuild CFG
buildMethod AST.MethodDecl {sig = sig, block = block@(AST.Block _ stmts sid)} = do
  checkStmts
  tail <- createEmptyBB
  setFunctionTail $ Just tail
  (blockH, blockT) <- buildBlock block
  updateCFG (G.addEdge blockT tail SeqEdge)
  -- TODO: Keep empty nodes for now to ease debugging.
  -- removeEmptySeqNode
  gets cfg

buildBlock :: AST.Block -> CFGBuild (BBID, BBID)
buildBlock block@AST.Block {stmts = stmts} = do
  checkStmts
  -- always create an empty BB at the start
  head <- createEmptyBB
  -- record parent scope
  parentScope <- use #astScope
  -- enter new block scope
  let scopeID = block ^. #blockID
  setASTScope scopeID
  -- create a new varmap for this scope
  #sym2var %= Map.insert scopeID (SymVarMap Map.empty $ Just parentScope)
  -- handle variable declarations
  mapM_ (\(AST.FieldDecl name tpe loc) -> newVar (Just name) tpe loc) (block ^. #vars)
  -- handle statements
  stmtT <- foldM (\_ s -> buildStatement s) (StayIn head) stmts
  -- connect last basic block with dangling statements if necessary
  tail <- case stmtT of 
    Deadend -> createEmptyBB
    StayIn bbid -> 
      -- collect dangling statements, possibly left by some previous basic block.
      finishCurrentBB
    TailAt bbid -> return bbid
  -- recover parent scope
  setASTScope parentScope
  return (head, tail)

buildAssignOp :: Var -> AST.AssignOp -> Maybe AST.Expr -> CFGBuild Var
buildAssignOp prev AST.EqlAssign (Just expr) = buildExpr expr
buildAssignOp prev AST.IncAssign (Just expr) = do
  addition <- buildExpr expr
  dst' <- newVar Nothing (SSA.tpe prev) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Plus (Variable prev) (Variable addition))
  return dst'
buildAssignOp prev AST.DecAssign (Just expr) = do
  addition <- buildExpr expr
  dst' <- newVar Nothing (SSA.tpe prev) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Minus (Variable prev) (Variable addition))
  return dst'
buildAssignOp prev AST.PlusPlus Nothing = do
  dst' <- newVar Nothing (SSA.tpe prev) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Plus (Variable prev) (IntImm 1))
  return dst'
buildAssignOp prev AST.MinusMinus Nothing = do
  dst' <- newVar Nothing (SSA.tpe prev) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Minus (Variable prev) (IntImm 1))
  return dst'
buildAssignOp _ _ _ = throwError $ CFGExcept "Malformed assignment."

buildStatement :: AST.Statement -> CFGBuild BBTransition
-- treat assignment to a scalar as creating a new variable
buildStatement (AST.Statement (AST.AssignStmt (AST.Assignment location@(AST.Location name Nothing def tpe loc) op expr _)) _) = do
  prev <- buildLocation location
  var <- buildAssignOp prev op expr
  dst <- newVar (Just name) tpe loc
  addSSA $ Assignment dst (Variable var)
  use #currentBBID <&> StayIn
-- treat assignment to an array element as a store.
buildStatement (AST.Statement (AST.AssignStmt (AST.Assignment location@(AST.Location name (Just idxE) def tpe loc) op expr _)) _) = do
  prev <- buildLocation location
  var <- buildAssignOp prev op expr
  array <- findVarOfSym' name
  idx <- buildExpr idxE
  addSSA $ Store array (Variable idx) (Variable var)
  use #currentBBID <&> StayIn
buildStatement (AST.Statement (AST.MethodCallStmt call) _) = do
  buildMethodCall call AST.Void
  use #currentBBID <&> StayIn
buildStatement (AST.Statement (AST.IfStmt expr ifBlock maybeElseBlock) loc) = do
  predVar <- buildExpr expr
  prevBB <- finishCurrentBB
  (ifHead, ifTail) <- buildBlock ifBlock
  g <- use #cfg
  do updateCFG (G.addEdge prevBB ifHead $ CondEdge $ Pred $ Variable predVar)
  case maybeElseBlock of
    (Just elseBlock) -> do
      (elseHead, elseTail) <- buildBlock elseBlock
      checkStmts
      tail <- createEmptyBB
      updateCFG $ do
        G.addEdge prevBB elseHead $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
        G.addEdge elseTail tail SeqEdge
      return $ TailAt tail
    Nothing -> do
      checkStmts
      tail <- createEmptyBB
      updateCFG $ do
        G.addEdge prevBB tail $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
      return $ TailAt tail
buildStatement (AST.Statement (AST.ForStmt counter init pred update block) loc) = do
  var <- findVarOfSym' counter
  initExpr <- buildExpr init
  addSSA $ Assignment var (Variable initExpr)
  prevBB <- finishCurrentBB
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG (G.addEdge prevBB predBB SeqEdge)
  buildStatement $ AST.Statement (AST.AssignStmt update) (update ^. #loc)
  updateBB <- finishCurrentBB
  parentControlBlock <- getControlBlock
  tail <- createEmptyBB
  setControlBlock $ Just (updateBB, tail)
  (blockHead, blockTail) <- buildBlock block
  checkStmts
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge blockTail updateBB SeqEdge
    G.addEdge updateBB predBB SeqEdge
    G.addEdge predBB tail $ CondEdge Complement
  setControlBlock parentControlBlock
  return $ TailAt tail
buildStatement (AST.Statement (AST.WhileStmt pred block) loc) = do
  prevBB <- finishCurrentBB
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG (G.addEdge prevBB predBB SeqEdge)
  parentControlBlock <- getControlBlock
  tail <- createEmptyBB
  setControlBlock $ Just (predBB, tail)
  (blockHead, blockTail) <- buildBlock block
  checkStmts
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge predBB tail $ CondEdge Complement
    G.addEdge blockTail tail SeqEdge
  setControlBlock parentControlBlock
  return $ TailAt tail
buildStatement (AST.Statement (AST.ReturnStmt expr') loc) = do
  ret <- case expr' of
    (Just e) -> buildExpr e
    _ -> newVar Nothing AST.Void loc
  addSSA $ Return ret
  -- Go directly to function exit
  bbid <- finishCurrentBB
  funcTail <- getFunctionTail
  case funcTail of
    Nothing -> throwError $ CFGExcept "Return not in a function context."
    Just tail -> updateCFG (G.addEdge bbid tail SeqEdge)
  -- Create an unreachable bb in case there are still statements after
  -- this point.
  -- We could optimize unreachable blocks away in later passes.
  createEmptyBB
  return Deadend
buildStatement (AST.Statement AST.ContinueStmt loc) = do
  bbid <- finishCurrentBB
  controlH' <- getControlEntry
  case controlH' of
    Nothing -> throwError $ CFGExcept "Continue not in a loop context."
    Just controlH -> updateCFG (G.addEdge bbid controlH SeqEdge)
  createEmptyBB
  return Deadend
buildStatement (AST.Statement AST.BreakStmt loc) = do
  bbid <- finishCurrentBB
  controlT' <- getControlExit
  case controlT' of
    Nothing -> throwError $ CFGExcept "Break not in a loop context."
    Just controlT -> updateCFG (G.addEdge bbid controlT SeqEdge)
  createEmptyBB
  return Deadend

buildExpr :: AST.Expr -> CFGBuild Var
buildExpr (AST.Expr (AST.LocationExpr location) tpe _) = buildLocation location
buildExpr (AST.Expr (AST.MethodCallExpr call) tpe _) = buildMethodCall call tpe
buildExpr (AST.Expr (AST.ExternCallExpr name args) tpe loc) =
  buildMethodCall (AST.MethodCall name args loc) tpe
buildExpr (AST.Expr (AST.IntLiteralExpr val) tpe loc) = do
  dst <- newVar Nothing tpe loc
  addSSA $ Assignment dst (IntImm val)
  return dst
buildExpr (AST.Expr (AST.BoolLiteralExpr val) tpe loc) = do
  dst <- newVar Nothing tpe loc
  addSSA $ Assignment dst (BoolImm val)
  return dst
buildExpr (AST.Expr (AST.CharLiteralExpr val) tpe loc) = do
  dst <- newVar Nothing tpe loc
  addSSA $ Assignment dst (CharImm val)
  return dst
buildExpr (AST.Expr (AST.StringLiteralExpr val) tpe loc) = do
  dst <- newVar Nothing tpe loc
  addSSA $ Assignment dst (StringImm val)
  return dst
buildExpr (AST.Expr (AST.ArithOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newVar Nothing tpe loc
  addSSA $ Arith dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.RelOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newVar Nothing tpe loc
  addSSA $ Rel dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.CondOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newVar Nothing tpe loc
  addSSA $ Cond dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.EqOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newVar Nothing tpe loc
  addSSA $ Eq dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.NegOpExpr op opr) tpe loc) = do
  opr' <- buildExpr opr
  dst <- newVar Nothing tpe loc
  addSSA $ Neg dst op (Variable opr')
  return dst
buildExpr (AST.Expr (AST.NotOpExpr op opr) tpe loc) = do
  opr' <- buildExpr opr
  dst <- newVar Nothing tpe loc
  addSSA $ Not dst op (Variable opr')
  return dst
buildExpr (AST.Expr (AST.ChoiceOpExpr op e1 e2 e3) tpe loc) = do
  e1' <- buildExpr e1
  e2' <- buildExpr e2
  e3' <- buildExpr e3
  dst <- newVar Nothing tpe loc
  addSSA $ Choice dst op (Variable e1') (Variable e2') (Variable e3')
  return dst
buildExpr (AST.Expr (AST.LengthExpr name) tpe loc) = do
  array <- findVarOfSym' name
  arrType <- case astDecl array of
    Nothing -> throwError $ CFGExcept "Unable to find array def"
    Just (Left (AST.Argument _ arrTpe _)) -> return arrTpe
    Just (Right (AST.FieldDecl _ arrTpe _)) -> return arrTpe
  case arrType of
    AST.ArrayType _ len -> do
      dst <- newVar Nothing tpe loc
      addSSA $ Assignment dst (IntImm len)
      return dst
    _ ->
      throwError $
        CFGExcept (sformat ("Variable is not an array: " % stext) name)

buildLocation :: AST.Location -> CFGBuild Var
buildLocation (AST.Location name Nothing def tpe loc) = findVarOfSym' name
buildLocation (AST.Location name (Just expr) def tpe loc) = do
  idx <- buildExpr expr
  array <- findVarOfSym' name
  dst <- newVar (Just name) tpe loc
  addSSA $ ArrayDeref dst array (Variable idx)
  return dst

buildMethodCall :: AST.MethodCall -> AST.Type -> CFGBuild Var
buildMethodCall (AST.MethodCall name args loc) tpe = do
  vars <- mapM buildExpr args
  dst <- newVar Nothing tpe loc
  addSSA $ MethodCall dst name vars
  return dst

-- Generate a dot plot for cfg

prettyPrintNode :: CFGNode -> Text
prettyPrintNode CFGNode {bb = BasicBlock {bbid = id, statements = stmts}} =
  let idText = [sformat ("id: " % int) id]
      segments = stmts <&> \s -> sformat shown s
   in Text.intercalate "\n" $ idText ++ segments

escape :: Text -> Text
escape str =
  Text.concatMap
    ( \w -> case w of
        '\\' -> "\\\\"
        '"' -> "\\\""
        c -> Text.singleton c
    )
    str

prettyPrintEdge :: CFGEdge -> Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = sformat shown var
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: G.Graph BBID CFGNode CFGEdge -> Text
generateDotPlot G.Graph {nodes = nodes, edges = edges} =
  let preamble = "digraph G {\n"
      postamble = "}"
      nodeBoxes = Map.assocs nodes <&> uncurry nodeBox
      edgeLines =
        concatMap
          (\(from, tos) -> tos <&> \(to, d) -> edgeLine from to d)
          $ Map.assocs edges
   in mconcat $ [preamble] ++ nodeBoxes ++ edgeLines ++ [postamble]
  where
    nodeBox idx d =
      sformat
        (int % " [shape=box, label=\"" % stext % "\"];\n")
        idx
        (escape (prettyPrintNode d))
    edgeLine from to d =
      sformat
        (int % " -> " % int % " [label=\"" % stext % "\"];\n")
        from
        to
        (escape (prettyPrintEdge d))

plot :: AST.ASTRoot -> Map ScopeID SE.SymbolTable -> Either [String] String
plot root st =
  let context = CFGContext st
      res = buildCFG root context
   in case res of
        Left (CFGExcept msg) -> Left [Text.unpack msg]
        Right cfgs -> Right $ Text.unpack $ mconcat $ Map.elems cfgs <&> generateDotPlot
