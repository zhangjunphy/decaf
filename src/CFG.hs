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

-- CFG -- Control Flow Graph with SSA nodes
module CFG where

import AST qualified
import Control.Applicative (liftA2, (<|>))
import Control.Exception (throw)
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
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
import Debug.Trace
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic qualified as SE
import Types
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL

{-
CFG creation.
TODO:
1. Implement Phi for divergent control flows. [DONE]
2. Merge all binary ops into one SSA [DROP]
3. ArrayDeref -> Load? [DONE]
4. Do we really need Len here? [DONE]
5. Reading from/Writing to global should work as load/store. [DONE]
6. Use control flow instead of choice. [DONE]
7. Handle short circuit expressions. [DONE]
8. Make sure everything works after compliating expr building with control flow. [DONE]?
9. Ensure bbid are monotonically-increasing with respect to ast statement ordering. [DROP]
-}

{-
Refactor and clean up.
TODO:
1. Find better ways to add phi nodes.
2. Refactor control start/exit related code.
3. Produce dot plot with some proper library.
4. Add unit tests.
5. Other chores.
-}

data Condition
  = Pred {pred :: !VarOrImm}
  | Complement
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: !BBID,
    sid :: !ScopeID,
    statements :: ![SSA]
  }
  deriving (Generic, Show)

newtype CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge !Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge

data SymVarMap = SymVarMap
  { m :: !(Map Name VID),
    parent :: !(Maybe ScopeID)
  }
  deriving (Show, Generic)

data CFGState = CFGState
  { cfg :: !CFG,
    astScope :: !ScopeID,
    sig :: !AST.MethodSig,
    currentBBID :: !BBID,
    vars :: !VarList,
    sym2var :: !(Map ScopeID SymVarMap),
    var2sym :: !(Map VID (ScopeID, Name)),
    statements :: ![SSA],
    currentControlBlock :: !(Maybe (BBID, BBID)), -- entry and exit
    currentFunctionTail :: !(Maybe BBID),
    varWrite :: !(Map BBID (Map (ScopeID, Name) Var))
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
    Map.empty

-- Helps for CFGBuild monad
updateCFG :: G.GraphBuilder BBID CFGNode CFGEdge a -> CFGBuild ()
updateCFG update = do
  g <- use #cfg
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CFGExcept m
    Right g -> #cfg .= g

data CFGContext = CFGContext
  {semantic :: SE.SemanticInfo}
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

getBasicBlock' :: BBID -> CFGBuild BasicBlock
getBasicBlock' bbid = do
  g <- use #cfg
  case G.lookupNode bbid g of
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find basic block" %+ int) bbid
    Just node -> return $ node ^. #bb

addVarSym :: Name -> VID -> CFGBuild ()
addVarSym name vid = do
  sid <-
    getSymScope name >>= \case
      Nothing -> throwError $ CFGExcept $ sformat ("Unable to find symbol" %+ stext) name
      Just sid -> return sid
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

lookupVar' :: VID -> CFGBuild (ScopeID, Name)
lookupVar' vid =
  lookupVar vid >>= \case
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable" %+ int) vid
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
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find symbol" %+ stext %+ "in scope" %+ int) name sid
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
  sig <- use #sig
  case Map.lookup sid sts of
    Nothing -> throwError $ CFGExcept (sformat ("Unable to find scope " % int) sid)
    Just st -> return $ lookup name st
  where
    lookup name st' =
      (SE.lookupLocalVariableFromST name st')
        <|> (SE.parent st' >>= lookup name)

getSymScope :: Name -> CFGBuild (Maybe ScopeID)
getSymScope name = do
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  sig <- use #sig
  case Map.lookup sid sts of
    Nothing -> throwError $ CFGExcept (sformat ("Unable to find scope " % int) sid)
    Just st -> return $ lookup name st
  where
    lookup name st' =
      (SE.lookupLocalVariableFromST name st' <&> \_ -> view #scopeID st')
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
    bridgeEdges mid inEdges (out, _) = do
      G.deleteNode mid
      forM_ inEdges (\(ni, ed) -> G.addEdge ni out ed)
    isSeqEdge SeqEdge = True
    isSeqEdge _ = False

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
  when (isNothing decl) $ throwError (CFGExcept $ sformat ("Unable to find decl of variable " % stext) name)
  let var = Var vid tpe decl sl locality
  #vars .= vars ++ [var]
  addVarSym name vid
  return var

newLocal :: Maybe Name -> AST.Type -> SL.Range -> CFGBuild Var
newLocal name tpe@(AST.ArrayType _ _) sl = do
  ptr <- newVar name tpe sl Local
  addSSA $ Alloca ptr tpe
  return ptr
newLocal name tpe sl = newVar name tpe sl Local

newGlobal :: Name -> AST.Type -> SL.Range -> CFGBuild Var
newGlobal name tpe@(AST.ArrayType _ _) sl = do
  sid <- use #astScope
  setASTScope 0
  ptr <- newVar (Just name) tpe sl Global
  addSSA $ Alloca ptr tpe
  setASTScope sid
  return ptr
newGlobal name tpe sl = do
  sid <- use #astScope
  setASTScope 0
  ptr <- newVar (Just name) (AST.Ptr tpe) sl Global
  addSSA $ Alloca ptr tpe
  setASTScope sid
  return ptr

addSSA :: SSA -> CFGBuild ()
addSSA ssa = do
  stmts <- use #statements
  #statements .= stmts ++ [ssa]

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

{----------------------------------------
Build cfg from ast fragments
-----------------------------------------}

populateGlobals :: AST.ASTRoot -> CFGBuild ()
populateGlobals root@(AST.ASTRoot _ globals _) = do
  sid <- use #astScope
  setASTScope 0
  mapM_ (\(AST.FieldDecl name tpe loc) -> newGlobal name tpe loc) globals
  finishCurrentBB
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
  mapM_ (\(AST.FieldDecl name tpe loc) -> newLocal (Just name) tpe loc) (block ^. #vars)
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

findOuterScopes :: ScopeID -> Map ScopeID SE.SymbolTable -> Set ScopeID
findOuterScopes scope sts = Set.fromList $ lookup scope sts
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

inferPhiList :: [ScopeID] -> Set ScopeID -> CFGBuild [(ScopeID, Name)]
inferPhiList divergence outer = do
  varWrites <- view $ #semantic . #symbolWrites
  let varList =
        filter (\(sid, _) -> Set.member sid outer) $
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
        Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable" %+ stext %+ "in scope" %+ int) name sid
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
  g <- use #cfg
  case G.lookupNode bb g of
    Nothing -> throwError $ CFGExcept $ sformat ("Basic block" %+ int %+ "not found.") bb
    Just node -> do
      let ssaList = node ^. (#bb . #statements)
      ssaList' <- mapM patch ssaList
      let node' = node & (#bb . #statements) .~ ssaList'
      #cfg %= G.updateNode bb node'
  where
    patch :: SSA -> CFGBuild SSA
    patch (Phi dst []) = do
      (sid, name) <- lookupVar' (dst ^. #id)
      let v1 = Map.lookup (sid, name) varMap1
      let v2 = Map.lookup (sid, name) varMap2
      case v1 of
        Nothing -> throwError $ CFGExcept $ sformat ("Unable to find symbol" %+ stext) name
        Just v1' -> do
          case v2 of
            Nothing -> throwError $ CFGExcept $ sformat ("Unable to find symbol" %+ stext) name
            Just v2' -> return $ Phi dst [(v1', s1), (v2', s2)]
    patch ssa = return ssa

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
buildAssignOp _ _ _ = throwError $ CFGExcept "Malformed assignment."

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
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  let outerScopes = findOuterScopes sid sts
  phiList <- inferPhiList phiBlocks outerScopes
  predVar <- buildExpr expr
  prevBB <- finishCurrentBB
  prevBBPhiList <- recordPhiVar phiList
  (ifHead, ifTail) <- buildBlock ifBlock
  ifBBPhiList <- recordPhiVar phiList
  g <- use #cfg
  updateCFG (G.addEdge prevBB ifHead $ CondEdge $ Pred $ Variable predVar)
  case maybeElseBlock of
    (Just elseBlock) -> do
      (elseHead, elseTail) <- buildBlock elseBlock
      elseBBPhiList <- recordPhiVar phiList
      checkStmts
      addDummyPhiNode phiList
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB elseHead $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
        G.addEdge elseTail tail SeqEdge
      patchPhiNode tail ifTail ifBBPhiList elseTail elseBBPhiList
      return $ TailAt tail
    Nothing -> do
      checkStmts
      sid <- use #astScope
      sts <- view $ #semantic . #symbolTables
      let outerScopes = findOuterScopes sid sts
      addDummyPhiNode phiList
      tail <- finishCurrentBB
      updateCFG $ do
        G.addEdge prevBB tail $ CondEdge Complement
        G.addEdge ifTail tail SeqEdge
      patchPhiNode tail prevBB prevBBPhiList ifTail ifBBPhiList
      return $ TailAt tail
buildStatement (AST.Statement (AST.ForStmt counter init pred update block) loc) = do
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  let outerScopes = findOuterScopes sid sts
  phiList <- inferPhiList [block ^. #blockID] outerScopes
  var <- lookupSym' counter
  initExpr <- buildExpr init
  case var ^. #astDecl of
    Nothing -> throwError $ CFGExcept "Counter var not found in symbol table."
    (Just decl) -> buildWriteToLocation (AST.Location counter Nothing decl (var ^. #tpe) loc) (Variable initExpr)
  prevBB <- finishCurrentBB
  prevBBPhiList <- recordPhiVar phiList
  addDummyPhiNode phiList
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG (G.addEdge prevBB predBB SeqEdge)
  dummyUpdateBB <- createEmptyBB
  parentControlBlock <- getControlBlock
  tail <- createEmptyBB
  setControlBlock $ Just (dummyUpdateBB, tail)
  (blockHead, blockTail) <- buildBlock block
  checkStmts
  buildStatement $ AST.Statement (AST.AssignStmt update) (update ^. #loc)
  updateBB <- finishCurrentBB
  dummyNode <- use #cfg <&> fromJust . G.lookupNode dummyUpdateBB
  updateNode <- use #cfg <&> fromJust . G.lookupNode updateBB
  updateBBPhiList <- recordPhiVar phiList
  #cfg
    %= G.updateNode
      dummyUpdateBB
      (dummyNode & #bb . #statements .~ (updateNode ^. #bb . #statements))
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge blockTail dummyUpdateBB SeqEdge
    G.addEdge dummyUpdateBB predBB SeqEdge
    G.addEdge predBB tail $ CondEdge Complement
    G.deleteNode updateBB
  setControlBlock parentControlBlock
  patchPhiNode predBB prevBB prevBBPhiList dummyUpdateBB updateBBPhiList
  return $ TailAt tail
buildStatement (AST.Statement (AST.WhileStmt pred block) loc) = do
  sid <- use #astScope
  sts <- view $ #semantic . #symbolTables
  let outerScopes = findOuterScopes sid sts
  phiList <- inferPhiList [block ^. #blockID] outerScopes
  prevBB <- finishCurrentBB
  prevBBPhiList <- recordPhiVar phiList
  addDummyPhiNode phiList
  predVar <- buildExpr pred
  predBB <- finishCurrentBB
  updateCFG $ G.addEdge prevBB predBB SeqEdge
  parentControlBlock <- getControlBlock
  tail <- createEmptyBB
  setControlBlock $ Just (predBB, tail)
  (blockHead, blockTail) <- buildBlock block
  checkStmts
  blockBBPhiList <- recordPhiVar phiList
  updateCFG $ do
    G.addEdge predBB blockHead $ CondEdge $ Pred $ Variable predVar
    G.addEdge predBB tail $ CondEdge Complement
    G.addEdge blockTail predBB SeqEdge
  setControlBlock parentControlBlock
  patchPhiNode predBB prevBB prevBBPhiList blockTail blockBBPhiList
  return $ TailAt tail
buildStatement (AST.Statement (AST.ReturnStmt expr') loc) = do
  ret <- case expr' of
    (Just e) -> buildExpr e
    _ -> newLocal Nothing AST.Void loc
  addSSA $ Return ret
  -- Go directly to function exit
  bbid <- finishCurrentBB
  funcTail <- getFunctionTail
  case funcTail of
    Nothing -> throwError $ CFGExcept "Return not in a function context."
    Just tail -> updateCFG $ G.addEdge bbid tail SeqEdge
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
    Just controlH -> updateCFG $ G.addEdge bbid controlH SeqEdge
  createEmptyBB
  return Deadend
buildStatement (AST.Statement AST.BreakStmt loc) = do
  bbid <- finishCurrentBB
  controlT' <- getControlExit
  case controlT' of
    Nothing -> throwError $ CFGExcept "Break not in a loop context."
    Just controlT -> updateCFG $ G.addEdge bbid controlT SeqEdge
  createEmptyBB
  return Deadend

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
buildExpr (AST.Expr (AST.StringLiteralExpr val) tpe loc) = do
  dst <- newLocal Nothing tpe loc
  addSSA $ Assignment dst (StringImm val)
  return dst
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
    Nothing -> throwError $ CFGExcept "Unable to find array def"
    Just (Left (AST.Argument _ arrTpe _)) -> return arrTpe
    Just (Right (AST.FieldDecl _ arrTpe _)) -> return arrTpe
  case arrType of
    AST.ArrayType _ len -> do
      dst <- newLocal Nothing tpe loc
      addSSA $ Assignment dst (IntImm len)
      return dst
    _ ->
      throwError $
        CFGExcept (sformat ("Variable is not an array: " % stext) name)

buildReadFromLocation :: AST.Location -> CFGBuild Var
buildReadFromLocation (AST.Location name idx def tpe loc) = do
  var <- lookupSym' name
  -- NOTE: var^. #tpe might be different from tpe in AST.Location as we
  -- have altered type of global into pointers.
  case var ^. #tpe of
    AST.ArrayType eleType size -> do
      case idx of
        Nothing -> throwError $ CFGExcept "Accessing array as a scalar."
        Just idxExpr -> do
          idxVar <- buildExpr idxExpr
          arrayPtr <- lookupSym' name
          eleSize <- case AST.dataSize eleType of
            Nothing -> throwError $ CFGExcept "Array with void type element."
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

recordVarWrite :: Var -> ScopeID -> Name -> CFGBuild ()
recordVarWrite var scope name = do
  bbid <- use #currentBBID
  #varWrite %= Map.insertWith Map.union bbid (Map.singleton (scope, name) var)

buildWriteToLocation :: AST.Location -> VarOrImm -> CFGBuild ()
buildWriteToLocation (AST.Location name idx def tpe loc) src = do
  var <- lookupSym' name
  case var ^. #tpe of
    AST.ArrayType eleType size ->
      case idx of
        Nothing -> throwError $ CFGExcept "Accessing array as a scalar."
        Just idxExpr -> do
          idx <- buildExpr idxExpr
          arrayPtr <- lookupSym' name
          eleSize <- case AST.dataSize eleType of
            Nothing -> throwError $ CFGExcept "Array with void type element."
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
      -- Record writes to scalars for phi node creation.
      (scope, symName) <- lookupVar' (var ^. #id)
      recordVarWrite dst scope symName
      addSSA $ Assignment dst src

buildMethodCall :: AST.MethodCall -> AST.Type -> CFGBuild Var
buildMethodCall (AST.MethodCall name args loc) tpe = do
  vars <- mapM buildExpr args
  dst <- newLocal Nothing tpe loc
  addSSA $ MethodCall dst name vars
  return dst

-- Generate a dot plot for cfg

prettyPrintNode :: CFGNode -> Text
prettyPrintNode CFGNode {bb = BasicBlock {bbid = id, statements = stmts}} =
  let idText = [sformat ("id: " % int) id]
      segments = stmts <&> \s -> sformat shown s
   in Text.intercalate "\n" $ idText ++ segments

escape :: Text -> Text
escape =
  Text.concatMap
    ( \case
        '\\' -> "\\\\"
        '"' -> "\\\""
        c -> Text.singleton c
    )

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
          (\(from, tos) -> tos <&> uncurry (edgeLine from))
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

plot :: AST.ASTRoot -> SE.SemanticInfo -> Either [String] String
plot root si =
  let context = CFGContext si
      res = buildCFG root context
   in case res of
        Left (CFGExcept msg) -> Left [Text.unpack msg]
        Right cfgs -> Right $ Text.unpack $ mconcat $ Map.elems cfgs <&> generateDotPlot
