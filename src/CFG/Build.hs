-- Copyright (C) 2018-2024 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

module CFG.Build where

import AST qualified
import CFG.Types
import Control.Applicative (liftA2, (<|>))
import Control.Exception (throw)
import Control.Lens (use, uses, view, views, (%=), (%~), (&), (+=), (.=), (.~), (<~), (^.), _1, _2, _3, _Just)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Generics.Labels
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic qualified as SE
import Types
import Util.Constants (globalScopeID)
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL

{------------------------------------------------
Data types
------------------------------------------------}
data SymVarMap = SymVarMap
  { m :: !(Map Name VID),
    parent :: !(Maybe ScopeID)
  }
  deriving (Show, Generic)

data CFGState = CFGState
  { cfg :: !(Maybe CFG),
    astScope :: !ScopeID,
    currentBBID :: !BBID,
    vars :: !VarList,
    sym2var :: !(Map ScopeID SymVarMap),
    var2sym :: !(Map VID (ScopeID, Name)),
    statements :: ![SSA],
    currentControlBlock :: !(Maybe (BBID, BBID)), -- entry and exit
    currentFunctionExit :: !(Maybe BBID),
    globalBB :: !(Maybe BasicBlock)
  }
  deriving (Generic)

data BBTransition
  = StayIn !BBID
  | TailAt !BBID
  | Deadend !BBID

initialState :: CFGState
initialState =
  CFGState
    { cfg = Nothing,
      astScope = 0,
      currentBBID = 0,
      vars = [],
      sym2var = Map.fromList [(0, SymVarMap Map.empty Nothing)],
      var2sym = Map.empty,
      statements = [],
      currentControlBlock = Nothing,
      currentFunctionExit = Nothing,
      globalBB = Nothing
    }

{------------------------------------------------
Helps for CFGBuild monad
------------------------------------------------}
getCFG :: CFGBuild CFG
getCFG =
  use #cfg >>= \case
    Nothing -> throwError $ CompileError Nothing "CFG not initialized"
    Just cfg -> return cfg

setCFG :: CFG -> CFGBuild ()
setCFG cfg = #cfg .= Just cfg

getGraph :: CFGBuild (G.Graph BBID BasicBlock CFGEdge)
getGraph = getCFG <&> view #graph

setGraph :: G.Graph BBID BasicBlock CFGEdge -> CFGBuild ()
setGraph g = #cfg . _Just . #graph .= g

updateCFG :: G.GraphBuilder BBID BasicBlock CFGEdge a -> CFGBuild ()
updateCFG update = do
  g <- getGraph
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CompileError Nothing m
    Right g -> setGraph g

data CFGContext = CFGContext
  {semantic :: SE.SemanticInfo}
  deriving (Generic)

newtype CFGBuild a = CFGBuild
  { runCFGBuild ::
      ExceptT
        CompileError
        (ReaderT CFGContext (State CFGState))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError CompileError,
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

withControlBlock :: BBID -> BBID -> CFGBuild a -> CFGBuild a
withControlBlock entry exit f = do
  prevCB <- getControlBlock
  setControlBlock $ Just (entry, exit)
  res <- f
  setControlBlock prevCB
  return res

{-----------------------------------------------------------
  Record function tail for return to find correct
  successor block.
------------------------------------------------------------}

setFunctionEntry :: BBID -> CFGBuild ()
setFunctionEntry entry = do
  #cfg . _Just . #entry .= entry

setFunctionExit :: BBID -> CFGBuild ()
setFunctionExit exit = do
  #cfg . _Just . #exit .= exit

getFunctionExit :: CFGBuild BBID
getFunctionExit = getCFG <&> view #exit

getBasicBlock' :: BBID -> CFGBuild BasicBlock
getBasicBlock' bbid = do
  g <- getGraph
  case G.lookupNode bbid g of
    Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find basic block" %+ int) bbid
    Just node -> return node

{-----------------------------------------------------------
Add/lookup symbols or variables
------------------------------------------------------------}
addVarSym :: Name -> VID -> CFGBuild ()
addVarSym name vid = do
  sid <-
    getSymScope name >>= \case
      Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find symbol" %+ stext) name
      Just sid -> return sid
  -- update var->sym
  #var2sym %= Map.insert vid (sid, name)
  -- update sym->var
  sym2var <- use #sym2var
  let sym2varInScope = Map.lookup sid sym2var
  case sym2varInScope of
    Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find scope" %+ int %+ "in sym->var map") sid
    Just s2v -> #sym2var %= Map.insert sid (s2v & #m %~ Map.insert name vid)

lookupVar :: VID -> CFGBuild (Maybe (ScopeID, Name))
lookupVar vid = uses #var2sym $ Map.lookup vid

lookupVar' :: VID -> CFGBuild (ScopeID, Name)
lookupVar' vid =
  lookupVar vid >>= \case
    Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find variable" %+ int) vid
    Just res -> return res

-- Look for symbol resursively
lookupSymInScope :: Name -> ScopeID -> CFGBuild (Maybe Var)
lookupSymInScope name sid = do
  sym2var <- use #sym2var
  vars <- use #vars
  let vid = Map.lookup sid sym2var >>= (\m' -> lookup name m' sym2var)
  return $ vid <&> (vars !!)
  where
    lookup :: Name -> SymVarMap -> Map ScopeID SymVarMap -> Maybe VID
    lookup name symVarMap sym2var = case Map.lookup name (symVarMap ^. #m) of
      Just vid -> Just vid
      Nothing ->
        (symVarMap ^. #parent)
          >>= (`Map.lookup` sym2var)
          >>= \map' -> lookup name map' sym2var

lookupSymInScope' :: Name -> ScopeID -> CFGBuild Var
lookupSymInScope' name sid = do
  var' <- lookupSymInScope name sid
  case var' of
    Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find symbol" %+ stext %+ "in scope" %+ int) name sid
    Just var -> return var

lookupSym :: Name -> CFGBuild (Maybe Var)
lookupSym name = do
  sid <- use #astScope
  lookupSymInScope name sid

lookupSym' :: Name -> CFGBuild Var
lookupSym' name = do
  sid <- use #astScope
  lookupSymInScope' name sid

getSymDecl :: Name -> CFGBuild (Maybe (Either AST.Argument AST.FieldDecl))
getSymDecl name = do
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  case Map.lookup sid sts of
    Nothing -> throwError $ CompileError Nothing (sformat ("Unable to find scope " % int) sid)
    Just st -> return $ lookup name st
  where
    lookup name st' =
      (SE.lookupLocalVariableFromST name st')
        <|> (SE.parent st' >>= lookup name)

getSymScope :: Name -> CFGBuild (Maybe ScopeID)
getSymScope name = do
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  case Map.lookup sid sts of
    Nothing -> throwError $ CompileError Nothing (sformat ("Unable to find scope " % int) sid)
    Just st -> return $ lookup name st
  where
    lookup name st' =
      (SE.lookupLocalVariableFromST name st' <&> \_ -> view #scopeID st')
        <|> (SE.parent st' >>= lookup name)

newVar :: Maybe Name -> AST.Type -> SL.Range -> Locality -> CFGBuild Var
newVar Nothing tpe sl locality = do
  vars <- use #vars
  let vid = length vars
  let var = Var vid tpe Nothing sl locality
  #vars .= vars ++ [var]
  return var
newVar (Just name) tpe sl locality = do
  vars <- use #vars
  let vid = length vars
  decl <- getSymDecl name
  when (isNothing decl) $ throwError (CompileError (Just sl) $ sformat ("Unable to find decl of variable " % stext) name)
  let var = Var vid tpe decl sl locality
  #vars .= vars ++ [var]
  addVarSym name vid
  return var

newLocal :: Maybe Name -> AST.Type -> SL.Range -> CFGBuild Var
newLocal name tpe@(AST.ArrayType _ _) sl = 
  traceShow tpe $ throwError (CompileError (Just sl) $ sformat ("Trying to create reg for an array: " % shown) name)
newLocal name tpe sl = newVar name tpe sl Local

allocaOnStack :: Maybe Name -> AST.Type -> SL.Range -> CFGBuild Var
allocaOnStack name tpe@(AST.ArrayType _ _) sl = do 
  ptr <- newVar name tpe sl Local
  addSSA $ Alloca ptr tpe
  return ptr
allocaOnStack name tpe sl = do 
  ptr <- newVar name (AST.Ptr tpe) sl Local
  addSSA $ Alloca ptr (AST.Ptr tpe)
  return ptr

newGlobal :: Name -> AST.Type -> SL.Range -> CFGBuild Var
newGlobal name tpe@(AST.ArrayType _ _) sl = do
  sid <- use #astScope
  setASTScope 0
  ptr <- newVar (Just name) tpe sl Global
  setASTScope sid
  return ptr
newGlobal name tpe sl = do
  sid <- use #astScope
  setASTScope 0
  ptr <- newVar (Just name) (AST.Ptr tpe) sl Global
  setASTScope sid
  return ptr

{-----------------------------------------------------------
Basic block manipulations
------------------------------------------------------------}

createEmptyBB :: CFGBuild BBID
createEmptyBB = do
  checkStmts
  #currentBBID += 1
  bbid <- use #currentBBID
  sid <- use #astScope
  let bb = BasicBlock bbid sid []
  updateCFG (G.addNode bbid bb)
  return bbid

finishCurrentBB :: CFGBuild BBID
finishCurrentBB = do
  bbid <- use #currentBBID
  stmts <- use #statements
  sid <- use #astScope
  #statements .= []
  let bb = BasicBlock bbid sid stmts
  updateCFG (G.addNode bbid bb)
  #currentBBID += 1
  return bbid

checkStmts :: CFGBuild ()
checkStmts = do
  stmts <- use #statements
  unless (null stmts) $ throwError $ CompileError Nothing $ Text.pack $ "Dangling statements found: " ++ show stmts

addSSA :: SSA -> CFGBuild ()
addSSA ssa = do
  stmts <- use #statements
  #statements .= stmts ++ [ssa]

{-----------------------------------------------------------
Functionalities to help adding phi nodes.

We do this in 4 steps:
1. Decide which symbols are required to be handled by phi nodes with inferPhiList.
   This relies on symbol modification information gathered in semantic analysis
   pass. Due to backward edges introduced by loops this information is
   not very convinient to get in the current module unless we add another pass.
2. Add dummy phi nodes at the start of control flow merging points.
3. For each symbol gathered in step 1, record which SSA var corresponds to it
   at the end each diverging control flow. The order of step 2 and 3 depends on
   the type of control flow we are dealing with.
4. Patch SSA var and its source basic block (from step 3) into dummy phi nodes
   added by step 2.
------------------------------------------------------------}

findOuterScopes :: CFGBuild (Set ScopeID)
findOuterScopes = do
  -- Set.fromList $ lookup scope sts
  scope <- use #astScope
  sts <- view (#semantic . #symbolTables)
  return $ Set.fromList $ lookup scope sts
  where
    lookup :: ScopeID -> Map ScopeID SE.SymbolTable -> [ScopeID]
    lookup scope sts =
      case Map.lookup scope sts of
        Nothing -> []
        Just st -> case view #parent st of
          Nothing -> [scope]
          Just parent ->
            let parentSID = view #scopeID parent
             in scope : lookup parentSID sts

inferPhiList :: [ScopeID] -> CFGBuild [(ScopeID, Name)]
inferPhiList divergence = do
  outerScopes <- findOuterScopes
  varWrites <- view $ #semantic . #symbolWrites
  let varList =
        filter (\(sid, _) -> Set.member sid outerScopes) $
          Set.toList $
            Set.unions $
              mapMaybe (`Map.lookup` varWrites) divergence
  return varList

addDummyPhiNode :: [(ScopeID, Name)] -> CFGBuild ()
addDummyPhiNode =
  mapM_
    ( \(sid, n) -> do
        (tpe, sl) <- getTypeAndSL sid n
        dst <- newLocal (Just n) tpe sl
        addSSA $ Phi dst []
    )
  where
    getTypeAndSL :: ScopeID -> Name -> CFGBuild (AST.Type, SL.Range)
    getTypeAndSL sid name = do
      sts <- view $ #semantic . #symbolTables
      case Map.lookup sid sts >>= SE.lookupLocalVariableFromST name of
        Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find variable" %+ stext %+ "in scope" %+ int) name sid
        Just def -> do
          let tpe = either (view #tpe) (view #tpe) def
          let sl = either (view #loc) (view #loc) def
          return (tpe, sl)

recordPhiVar :: [(ScopeID, Name)] -> CFGBuild (Map (ScopeID, Name) Var)
recordPhiVar symList = do
  varList <-
    mapM
      ( \(sid, n) -> do
          var <- lookupSymInScope' n sid
          return ((sid, n), var)
      )
      symList
  return $ Map.fromList varList

patchPhiNode :: BBID -> BBID -> Map (ScopeID, Name) Var -> BBID -> Map (ScopeID, Name) Var -> CFGBuild ()
patchPhiNode bb s1 varMap1 s2 varMap2 = do
  g <- getGraph
  case G.lookupNode bb g of
    Nothing -> throwError $ CompileError Nothing $ sformat ("Basic block" %+ int %+ "not found.") bb
    Just node -> do
      let ssaList = node ^. #statements
      ssaList' <- mapM patch ssaList
      updateCFG $ G.adjustNode bb (#statements .~ ssaList')
  where
    patch :: SSA -> CFGBuild SSA
    patch (Phi dst []) = do
      (sid, name) <- lookupVar' (dst ^. #id)
      let v1 = Map.lookup (sid, name) varMap1
      let v2 = Map.lookup (sid, name) varMap2
      case v1 of
        Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find symbol" %+ stext) name
        Just v1' -> do
          case v2 of
            Nothing -> throwError $ CompileError Nothing $ sformat ("Unable to find symbol" %+ stext) name
            Just v2' -> return $ Phi dst [(v1', s1), (v2', s2)]
    patch ssa = return ssa

{----------------------------------------
Build cfg from ast fragments
-----------------------------------------}

buildCFGs :: AST.ASTRoot -> CFGContext -> Either [CompileError] SingleFileCFG
buildCFGs root@(AST.ASTRoot _ _ methods) context =
  runMonads buildAll initialState
  where
    runMonads build state =
      let (res, _) = runState (flip runReaderT context $ runExceptT $ runCFGBuild build) state
       in case res of
            Left e -> Left [e]
            Right a -> Right a
    buildAll = do
      globalBB <- populateGlobals root
      cfgs <-
        sequence $
          Map.fromList
            (methods <&> \method -> (method ^. #sig . #name, buildMethod method))
      return $ SingleFileCFG globalBB cfgs

populateGlobals :: AST.ASTRoot -> CFGBuild BasicBlock
populateGlobals root@(AST.ASTRoot _ globals _) = do
  sid <- use #astScope
  setASTScope 0
  allocs <-
    mapM
      ( \(AST.FieldDecl name tpe loc) -> do
          ptr <- newGlobal name tpe loc
          return $ InitGlobal ptr tpe
      )
      globals
  setASTScope sid
  return $ BasicBlock 0 globalScopeID allocs

buildMethod :: AST.MethodDecl -> CFGBuild CFG
buildMethod decl@AST.MethodDecl {sig = sig, block = block@(AST.Block _ stmts sid)} = do
  checkStmts
  setCFG $ CFG G.empty 0 0 sig
  entry <- createEmptyBB
  setFunctionEntry entry
  exit <- createEmptyBB
  setFunctionExit exit
  (blockH, blockT) <- buildBlock (sig ^. #args) block
  updateCFG $ do
    G.addEdge entry blockH SeqEdge
    G.addEdge blockT exit SeqEdge
  getCFG

buildBlock :: [AST.Argument] -> AST.Block -> CFGBuild (BBID, BBID)
buildBlock args block@AST.Block {stmts = stmts} = do
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
  -- handle method arguments
  mapM_ (\(AST.Argument name tpe loc) -> newLocal (Just name) tpe loc) args
  -- handle variable declarations
  mapM_ (\(AST.FieldDecl name tpe loc) -> allocaOnStack (Just name) tpe loc) (block ^. #vars)
  -- handle statements
  stmtT <- foldM (\_ s -> buildStatement s) (StayIn head) stmts
  -- connect last basic block with dangling statements if necessary
  tail <- case stmtT of
    Deadend bbid -> do
      checkStmts
      return bbid
    StayIn bbid ->
      -- collect dangling statements, possibly left by some previous basic block.
      finishCurrentBB
    TailAt bbid -> do
      -- some basic blocks were created, we check for dangling statements
      checkStmts
      return bbid
  -- recover parent scope
  setASTScope parentScope
  return (head, tail)

buildAssignOp :: Var -> AST.AssignOp -> Maybe AST.Expr -> CFGBuild Var
buildAssignOp prev AST.EqlAssign (Just expr) = buildExpr expr
buildAssignOp prev AST.IncAssign (Just expr) = do
  addition <- buildExpr expr
  dst' <- newLocal Nothing (prev ^. #tpe) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Plus (Variable prev) (Variable addition))
  return dst'
buildAssignOp prev AST.DecAssign (Just expr) = do
  addition <- buildExpr expr
  dst' <- newLocal Nothing (prev ^. #tpe) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Minus (Variable prev) (Variable addition))
  return dst'
buildAssignOp prev AST.PlusPlus Nothing = do
  dst' <- newLocal Nothing (prev ^. #tpe) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Plus (Variable prev) (IntImm 1))
  return dst'
buildAssignOp prev AST.MinusMinus Nothing = do
  dst' <- newLocal Nothing (prev ^. #tpe) (SSA.loc prev)
  addSSA (SSA.Arith dst' AST.Minus (Variable prev) (IntImm 1))
  return dst'
buildAssignOp _ _ _ = throwError $ CompileError Nothing "Malformed assignment."

buildStatement :: AST.Statement -> CFGBuild BBTransition
buildStatement (AST.Statement (AST.AssignStmt (AST.Assignment location op expr _)) _) = do
  prev <- buildReadFromLocation location
  var <- buildAssignOp prev op expr
  buildWriteToLocation location (Variable var)
  use #currentBBID <&> StayIn
buildStatement (AST.Statement (AST.MethodCallStmt call) _) = do
  buildMethodCall call AST.Void
  use #currentBBID <&> StayIn
buildStatement (AST.Statement (AST.IfStmt expr ifBlock maybeElseBlock) loc) = do
  let phiBlocks = case maybeElseBlock of
        Nothing -> [ifBlock ^. #blockID]
        Just elseBlock -> [ifBlock ^. #blockID, elseBlock ^. #blockID]
  -- Finish previous basic block. Also append pred calculation to it.
  phiList <- inferPhiList phiBlocks
  predVar <- buildExpr expr
  prevBB <- finishCurrentBB
  prevBBPhiList <- recordPhiVar phiList
  -- Build if body
  (ifHead, ifTail) <- buildBlock [] ifBlock
  ifBBPhiList <- recordPhiVar phiList
  updateCFG (G.addEdge prevBB ifHead $ CondEdge $ Pred $ Variable predVar)
  -- Build else body if it exist.
  case maybeElseBlock of
    (Just elseBlock) -> do
      (elseHead, elseTail) <- buildBlock [] elseBlock
      elseBBPhiList <- recordPhiVar phiList
      addDummyPhiNode phiList
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB elseHead $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
        G.addEdge elseTail tail SeqEdge
      patchPhiNode tail ifTail ifBBPhiList elseTail elseBBPhiList
      return $ TailAt tail
    Nothing -> do
      addDummyPhiNode phiList
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB tail $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
      patchPhiNode tail prevBB prevBBPhiList ifTail ifBBPhiList
      return $ TailAt tail
buildStatement (AST.Statement (AST.ForStmt init pred update block) loc) = do
  phiList <- inferPhiList [block ^. #blockID]
  -- append init to previous block
  case init of
    Nothing -> return ()
    Just init -> void $ buildStatement $ AST.Statement (AST.AssignStmt init) (init ^. #loc)
  prevBB <- finishCurrentBB
  prevBBPhiList <- recordPhiVar phiList
  -- build the pred, also add dummy phi nodes at the start
  predHead <- use #currentBBID
  addDummyPhiNode phiList
  predVar <- buildExpr pred
  predTail <- finishCurrentBB
  updateCFG (G.addEdge prevBB predHead SeqEdge)
  -- create a dummy update block for continue to jump to
  dummyUpdateBB <- createEmptyBB
  -- create a dummy tail block fro break to jump to
  tail <- createEmptyBB
  -- build for body and block connections
  updateBBPhiList <-
    withControlBlock
      dummyUpdateBB
      tail
      ( do
          (blockHead, blockTail) <- buildBlock [] block
          -- create the actual update block(s)
          updateHead <- use #currentBBID
          case update of
            Nothing -> return ()
            Just update -> void $ buildStatement $ AST.Statement (AST.AssignStmt update) (update ^. #loc)
          updateTail <- finishCurrentBB
          updateBBPhiList <- recordPhiVar phiList
          updateCFG $ do
            G.addEdge predTail blockHead $ CondEdge $ Pred $ Variable predVar
            G.addEdge predTail tail $ CondEdge Complement
            G.addEdge blockTail dummyUpdateBB SeqEdge
            G.addEdge dummyUpdateBB updateHead SeqEdge
            G.addEdge updateTail predHead SeqEdge
          return updateBBPhiList
      )
  patchPhiNode predHead prevBB prevBBPhiList dummyUpdateBB updateBBPhiList
  return $ TailAt tail
buildStatement (AST.Statement (AST.ReturnStmt expr') loc) = do
  ret <- case expr' of
    (Just e) -> buildExpr e
    _ -> newLocal Nothing AST.Void loc
  addSSA $ Return ret
  -- Go directly to function exit
  bbid <- finishCurrentBB
  funcTail <- getFunctionExit
  case funcTail of
    0 -> throwError $ CompileError (Just loc) "Return not in a function context."
    tail -> updateCFG $ G.addEdge bbid tail SeqEdge
  -- Create an unreachable bb in case there are still statements after
  -- this point.
  -- We could optimize unreachable blocks away in later passes.
  createEmptyBB <&> Deadend
buildStatement (AST.Statement AST.ContinueStmt loc) = do
  bbid <- finishCurrentBB
  controlH' <- getControlEntry
  case controlH' of
    Nothing -> throwError $ CompileError (Just loc) "Continue not in a loop context."
    Just controlH -> updateCFG $ G.addEdge bbid controlH SeqEdge
  createEmptyBB <&> Deadend
buildStatement (AST.Statement AST.BreakStmt loc) = do
  bbid <- finishCurrentBB
  controlT' <- getControlExit
  case controlT' of
    Nothing -> throwError $ CompileError (Just loc) "Break not in a loop context."
    Just controlT -> updateCFG $ G.addEdge bbid controlT SeqEdge
  createEmptyBB <&> Deadend

buildExpr :: AST.Expr -> CFGBuild Var
buildExpr (AST.Expr (AST.LocationExpr location) tpe _) = buildReadFromLocation location
buildExpr (AST.Expr (AST.MethodCallExpr call) tpe _) = buildMethodCall call tpe
buildExpr (AST.Expr (AST.ExternCallExpr name args) tpe loc) =
  buildMethodCall (AST.MethodCall name args loc) tpe
buildExpr (AST.Expr (AST.IntLiteralExpr val) tpe loc) = do
  dst <- newLocal Nothing tpe loc
  addSSA $ Assignment dst (IntImm val)
  return dst
buildExpr (AST.Expr (AST.BoolLiteralExpr val) tpe loc) = do
  dst <- newLocal Nothing tpe loc
  addSSA $ Assignment dst (BoolImm val)
  return dst
buildExpr (AST.Expr (AST.CharLiteralExpr val) tpe loc) = do
  dst <- newLocal Nothing tpe loc
  addSSA $ Assignment dst (CharImm val)
  return dst
buildExpr (AST.Expr (AST.StringLiteralExpr val) _ loc) = do
  let len = fromIntegral (Text.length val) + 1
  let tpe = AST.ArrayType AST.CharType len
  ptr <- newVar Nothing tpe loc Local
  addSSA $ AllocaStr ptr (Text.append val "\0") tpe
  return ptr
buildExpr (AST.Expr (AST.ArithOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newLocal Nothing tpe loc
  addSSA $ Arith dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.RelOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newLocal Nothing tpe loc
  addSSA $ Rel dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.CondOpExpr AST.And lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  lhsTail <- finishCurrentBB
  rhsHead <- use #currentBBID
  rhs' <- buildExpr rhs
  rhsTail <- finishCurrentBB
  tail <- use #currentBBID
  dst <- newLocal Nothing tpe loc
  updateCFG $ do
    G.addEdge lhsTail rhsHead $ CondEdge $ Pred $ Variable lhs'
    G.addEdge lhsTail tail $ CondEdge Complement
    G.addEdge rhsTail tail SeqEdge
  addSSA $ Phi dst [(lhs', lhsTail), (rhs', rhsTail)]
  return dst
buildExpr (AST.Expr (AST.CondOpExpr AST.Or lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  lhsTail <- finishCurrentBB
  rhsHead <- use #currentBBID
  rhs' <- buildExpr rhs
  rhsTail <- finishCurrentBB
  bbid <- use #currentBBID
  dst <- newLocal Nothing tpe loc
  updateCFG $ do
    G.addEdge lhsTail bbid $ CondEdge $ Pred $ Variable lhs'
    G.addEdge lhsTail rhsHead $ CondEdge Complement
    G.addEdge rhsHead bbid SeqEdge
  addSSA $ Phi dst [(lhs', lhsTail), (rhs', rhsTail)]
  return dst
buildExpr (AST.Expr (AST.EqOpExpr op lhs rhs) tpe loc) = do
  lhs' <- buildExpr lhs
  rhs' <- buildExpr rhs
  dst <- newLocal Nothing tpe loc
  addSSA $ Eq dst op (Variable lhs') (Variable rhs')
  return dst
buildExpr (AST.Expr (AST.NegOpExpr op opr) tpe loc) = do
  opr' <- buildExpr opr
  dst <- newLocal Nothing tpe loc
  addSSA $ Neg dst op (Variable opr')
  return dst
buildExpr (AST.Expr (AST.NotOpExpr op opr) tpe loc) = do
  opr' <- buildExpr opr
  dst <- newLocal Nothing tpe loc
  addSSA $ Not dst op (Variable opr')
  return dst
buildExpr (AST.Expr (AST.ChoiceOpExpr op e1 e2 e3) tpe loc) = do
  pred' <- buildExpr e1
  prev <- finishCurrentBB
  exprT' <- buildExpr e2
  bbTHead <- use #currentBBID
  bbTTail <- finishCurrentBB
  bbFHead <- use #currentBBID
  exprF' <- buildExpr e3
  bbFTail <- finishCurrentBB
  tail <- use #currentBBID
  dst <- newLocal Nothing tpe loc
  updateCFG $ do
    G.addEdge prev bbTHead $ CondEdge $ Pred $ Variable pred'
    G.addEdge prev bbFHead $ CondEdge Complement
    G.addEdge bbTTail tail SeqEdge
    G.addEdge bbFTail tail SeqEdge
  addSSA $ Phi dst [(exprT', bbTTail), (exprF', bbFTail)]
  return dst
buildExpr (AST.Expr (AST.LengthExpr name) tpe loc) = do
  array <- lookupSym' name
  arrType <- case astDecl array of
    Nothing -> throwError $ CompileError (Just loc) "Unable to find array def"
    Just (Left (AST.Argument _ arrTpe _)) -> return arrTpe
    Just (Right (AST.FieldDecl _ arrTpe _)) -> return arrTpe
  case arrType of
    AST.ArrayType _ len -> do
      dst <- newLocal Nothing tpe loc
      addSSA $ Assignment dst (IntImm len)
      return dst
    _ ->
      throwError $
        CompileError (Just loc) (sformat ("Variable is not an array: " % stext) name)

buildReadFromLocation :: AST.Location -> CFGBuild Var
buildReadFromLocation (AST.Location name idx def tpe loc) = do
  var <- lookupSym' name
  -- NOTE: var^. #tpe might be different from tpe in AST.Location as we
  -- have altered type of global into pointers.
  case var ^. #tpe of
    AST.ArrayType eleType size -> do
      case idx of
        Nothing -> throwError $ CompileError (Just loc) "Accessing array as a scalar."
        Just idxExpr -> do
          idxVar <- buildExpr idxExpr
          arrayPtr <- lookupSym' name
          eleSize <- case AST.dataSize eleType of
            Nothing -> throwError $ CompileError (Just loc) "Array with void type element."
            Just sz -> return sz
          offset <- newLocal Nothing AST.IntType loc
          ptr <- newLocal Nothing AST.IntType loc
          dst <- newLocal Nothing eleType loc
          addSSA $ Arith offset AST.Multiply (Variable idxVar) (IntImm eleSize)
          addSSA $ Arith ptr AST.Plus (Variable arrayPtr) (Variable offset)
          addSSA $ Load dst (Variable ptr)
          return dst
    AST.Ptr tpe -> do
      dst <- newLocal Nothing tpe loc
      addSSA $ Load dst (Variable var)
      return dst
    _scalarType -> return var

buildWriteToLocation :: AST.Location -> VarOrImm -> CFGBuild ()
buildWriteToLocation (AST.Location name idx def tpe loc) src = do
  var <- lookupSym' name
  case var ^. #tpe of
    AST.ArrayType eleType size ->
      case idx of
        Nothing -> throwError $ CompileError (Just loc) "Accessing array as a scalar."
        Just idxExpr -> do
          idx <- buildExpr idxExpr
          arrayPtr <- lookupSym' name
          eleSize <- case AST.dataSize eleType of
            Nothing -> throwError $ CompileError (Just loc) "Array with void type element."
            Just sz -> return sz
          offset <- newLocal Nothing AST.IntType loc
          ptr <- newLocal Nothing AST.IntType loc
          addSSA $ Arith offset AST.Multiply (Variable idx) (IntImm eleSize)
          addSSA $ Arith ptr AST.Plus (Variable arrayPtr) (Variable offset)
          addSSA $ Store (Variable ptr) src
    AST.Ptr tpe -> do
      addSSA $ Store (Variable var) src
    _scalarType -> do
      dst <- newLocal (Just name) tpe loc
      addSSA $ Assignment dst src

buildMethodCall :: AST.MethodCall -> AST.Type -> CFGBuild Var
buildMethodCall (AST.MethodCall name args loc) tpe = do
  vars <- mapM buildExpr args
  dst <- newLocal Nothing tpe loc
  addSSA $ MethodCall dst name vars
  return dst
