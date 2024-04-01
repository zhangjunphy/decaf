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
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.), _1)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic qualified as SE
import Types
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL
import qualified Data.GraphViz.Types.Graph as GV

data VarBiMap = VarBiMap
  { varToSym :: Map VID (ScopeID, Name),
    symToVar :: Map (ScopeID, Name) VID
  }

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
    nextBBID :: BBID,
    vars :: VarList,
    symMap :: VarBiMap,
    statements :: [SSA]
  }
  deriving (Generic)

initialState :: CFGState
initialState = CFGState G.empty 0 0 [] (VarBiMap Map.empty Map.empty) []

-- Helps for CFGBuild monad
consumeBBID :: CFGBuild BBID
consumeBBID = do
  bbid <- use #nextBBID
  #nextBBID += 1
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

getVarSymbol :: Name -> CFGBuild AST.FieldDecl
getVarSymbol name = do
  sid <- use #astScope
  sts <- view #symbolTables
  let st = fromJust $ Map.lookup sid sts
  return $ fromJust $ Map.lookup name (st ^. #variableSymbols)

createIsolateBB :: CFGBuild BBID
createIsolateBB = do
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

newVar :: Maybe Name -> SL.Range -> AST.Type -> CFGBuild Var
newVar Nothing sl tpe = do
  vars <- use #vars
  let vid = length vars
  let var = Var vid tpe Nothing sl
  #vars .= vars ++ [var]
  return var
newVar (Just name) sl tpe = do
  vars <- use #vars
  let vid = length vars
  decl <- getVarSymbol name
  let var = Var vid tpe (Just decl) sl
  #vars .= vars ++ [var]
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
  let vid = lookupSym sid name varBiMap
  return $ vid <&> (vars !!)

findVarOfSym' :: Name -> CFGBuild Var
findVarOfSym' name = do
  var <- findVarOfSym name
  case var of
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable " % stext) name
    Just v -> return v

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
buildCFG (AST.ASTRoot _ _ methods) context =
  let build method =
        runState $
          flip runReaderT context $
            runExceptT $
              runCFGBuild (buildMethod method)
      updateMap map m =
        let (r, _) = build m $ initialCFGState $ m ^. (#block . #blockID)
         in case r of
              Left e -> Left e
              Right r' -> Right $ Map.insert (m ^. (#sig . #name)) r' map
   in foldM updateMap Map.empty methods


-- Build cfg from ast fragments

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
  head <- createIsolateBB
  stmtTail <- foldM buildStatement head stmts
  tail <-
    if stmtTail == head
      then do
        id <- createIsolateBB
        sid <- use #astScope
        updateCFG (G.addEdge head id $ SeqEdge sid)
        return id
      else return stmtTail
  return (head, tail)

buildStatement :: BBID -> AST.Statement -> CFGBuild BBID 
buildStatement prev (AST.Statement (AST.AssignStmt (AST.Assignment (AST.Location name Nothing def tpe loc) op (Just expr) _)) _) = do
  dst <- newVar (Just name) loc tpe
  src <- buildExpr expr
  addSSA $ Assignment dst (Variable src)
buildStatement (AST.Statement (AST.IfStmt expr _ _) loc) =  do
  let (AST.Expr _ predTpe predLoc) = expr
  predVar <- newVar Nothing predLoc predTpe
  addSSA $ Phi (Variable predVar)
buildStatement (AST.Statement (AST.ForStmt counter init pred update block) loc) =  do
  _

buildExpr :: AST.Expr -> CFGBuild Var
buildExpr (AST.Expr (AST.LocationExpr location) tpe _) = buildLocation location

buildLocation :: AST.Location -> CFGBuild Var
buildLocation (AST.Location name Nothing def tpe loc) = findVarOfSym' name
buildLocation (AST.Location name (Just expr) def tpe loc) = do
  idx <- buildExpr expr
  array <- findVarOfSym' name
  dst <- newVar (Just name) loc tpe
  addSSA $ ArrayDeref dst array (Variable idx)
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
prettyPrintEdge (SeqEdge) = ""
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
