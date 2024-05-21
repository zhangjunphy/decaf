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
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.), _1)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.GraphViz.Types.Graph qualified as GV
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic (lookupLocalMethodFromST)
import Semantic qualified as SE
import Types
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL
import Control.Exception (throw)
import Data.Generics.Labels

import Debug.Trace
import qualified AST

data VarBiMap = VarBiMap
  { varToSym :: Map VID (ScopeID, Name),
    symToVar :: Map (ScopeID, Name) VID
  } deriving (Show)

addVarSym :: ScopeID -> Name -> VID -> VarBiMap -> VarBiMap
addVarSym sid name vid (VarBiMap varToSym symToVar) =
  let varToSym' = Map.insert vid (sid, name) varToSym
      symToVar' = Map.insert (sid, name) vid symToVar
   in VarBiMap varToSym' symToVar'

lookupVar :: VID -> VarBiMap -> Maybe (ScopeID, Name)
lookupVar vid VarBiMap {varToSym = m} = Map.lookup vid m

lookupSym :: ScopeID -> Name -> VarBiMap -> Maybe VID
lookupSym sid name VarBiMap {symToVar = m} = Map.lookup (sid, name) m

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

data CFGState = CFGState
  { cfg :: CFG,
    astScope :: ScopeID,
    sig :: AST.MethodSig,
    currentBBID :: BBID,
    vars :: VarList,
    symMap :: VarBiMap,
    statements :: [SSA]
  }
  deriving (Generic)

initialState :: ScopeID -> AST.MethodSig -> CFGState
initialState sid sig = CFGState G.empty sid sig 0 [] (VarBiMap Map.empty Map.empty) []

-- Helps for CFGBuild monad
consumeBBID :: CFGBuild BBID
consumeBBID = do
  bbid <- use #currentBBID
  #currentBBID += 1
  return bbid

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

finishCurrentBB :: CFGBuild BBID
finishCurrentBB = do
  bbid <- consumeBBID
  stmts <- use #statements
  sid <- use #astScope
  #statements .= []
  let bb = BasicBlock bbid sid stmts
  updateCFG (G.addNode bbid (CFGNode bb))
  return bbid

checkStmts :: CFGBuild ()
checkStmts = do
  stmts <- use #statements
  unless (null stmts) $ throwError $ CFGExcept $ Text.pack $ "Dangling statements found: " ++ show stmts

removeEmptySeqNode :: CFGBuild ()
removeEmptySeqNode = do
  g@G.Graph {nodes = nodes} <- gets cfg
  let emptySeqNodes =
        filter
          (\(ni, nd) -> isEmptyNode nd && isSeqOut ni g)
          $ Map.assocs nodes
  let gUpdate =
        mapM_
          ( \(ni, _) -> do
              let inEdges = G.inBound ni g
                  outEdge = head $ G.outBound ni g
               in bridgeEdges ni inEdges outEdge
          )
          emptySeqNodes
  updateCFG gUpdate
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
  sid <- use #astScope
  when (isNothing decl) $ throwError (CFGExcept $ sformat ("Unable to find decl of variable " % stext) name)
  let var = Var vid tpe decl sl
  #vars .= vars ++ [var]
  symMap <- use #symMap
  #symMap .= addVarSym sid name vid symMap
  return var

addSSA :: SSA -> CFGBuild ()
addSSA ssa = do
  stmts <- use #statements
  #statements .= stmts ++ [ssa]

findVarOfSym :: Name -> CFGBuild (Maybe Var)
findVarOfSym name = do
  sid <- use #astScope
  varBiMap <- use #symMap
  vars <- use #vars
  sts <- view #symbolTables
  st <- case Map.lookup sid sts of
    Nothing -> throwError (CFGExcept $ sformat ("Unable to find scope " % int) sid)
    Just st -> return st
  let sidDef = lookupSid name st
  let vid = sidDef >>= \id -> lookupSym id name varBiMap
  return $ vid <&> (vars !!)
  where
    lookupSid name st' = (SE.lookupLocalVariableFromST name st' <&> \_ -> st' ^. #scopeID)
      <|> (SE.parent st' >>= lookupSid name)

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
        let (r, _) = build m $ initialState (m ^. (#block . #blockID)) (m ^. #sig)
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
  buildBlock block
  removeEmptySeqNode
  gets cfg

buildBlock :: AST.Block -> CFGBuild (BBID, BBID)
buildBlock block@AST.Block {stmts = stmts} = do
  checkStmts
  -- always create an empty BB at the start
  head <- finishCurrentBB
  current <- use #currentBBID
  -- handle variable definitions
  mapM_ (\(AST.FieldDecl name tpe loc) -> newVar (Just name) tpe loc) (block ^. #vars)
  -- handl statements
  stmtTail <- mapM buildStatement stmts <&> \ids -> case ids of
    [] -> current
    _ -> last ids
  tail <- if stmtTail == current
      then do
        id <- finishCurrentBB
        updateCFG (G.addEdge head id SeqEdge)
        return id
      else return stmtTail
  return (head, tail)

buildStatement :: AST.Statement -> CFGBuild BBID
buildStatement (AST.Statement (AST.AssignStmt (AST.Assignment location op (Just expr) _)) _) = do
  dst <- buildLocation location
  src <- buildExpr expr
  addSSA $ Assignment dst (Variable src)
  use #currentBBID
buildStatement (AST.Statement (AST.MethodCallStmt call) _) = do
  buildMethodCall call AST.Void
  use #currentBBID
buildStatement (AST.Statement (AST.IfStmt expr ifBlock maybeElseBlock) loc) = do
  predVar <- buildExpr expr
  prevBB <- finishCurrentBB
  (ifHead, ifTail) <- buildBlock ifBlock
  g <- use #cfg
  do updateCFG (G.addEdge prevBB ifHead $ CondEdge $ Pred $ Variable predVar)
  case maybeElseBlock of
    (Just elseBlock) -> do
      (elseHead, elseTail) <- buildBlock elseBlock
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB elseHead $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
        G.addEdge elseTail tail SeqEdge
      return tail
    Nothing -> do
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB tail $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
      return tail
buildStatement (AST.Statement (AST.ForStmt counter init pred update block) loc) =  do
  var <- findVarOfSym' counter
  initExpr <- buildExpr init
  addSSA $ Assignment var (Variable initExpr)
  prevBB <- finishCurrentBB
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG (G.addEdge prevBB predBB SeqEdge)
  (blockHead, blockTail) <- buildBlock block
  buildStatement $ AST.Statement (AST.AssignStmt update) (update ^. #loc)
  updateBB <- finishCurrentBB
  tail <- finishCurrentBB
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge blockTail updateBB SeqEdge
    G.addEdge updateBB predBB SeqEdge
    G.addEdge predBB tail $ CondEdge Complement
  return tail
buildStatement (AST.Statement (AST.WhileStmt pred block) loc) = do
  prevBB <- finishCurrentBB
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG (G.addEdge prevBB predBB SeqEdge)
  (blockHead, blockTail) <- buildBlock block
  tail <- finishCurrentBB
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge predBB tail $ CondEdge Complement
    G.addEdge blockTail tail SeqEdge
  return tail
buildStatement (AST.Statement (AST.ReturnStmt expr') loc) = do
  ret <- case expr' of
    (Just e) -> buildExpr e
    _ -> newVar Nothing AST.Void loc
  addSSA $ Return ret
  use #currentBBID

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
  dst <- newVar Nothing tpe loc
  vars <- mapM buildExpr args
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
