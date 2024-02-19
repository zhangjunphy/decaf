-- PartialCFG -- Control Flow Graph with unconverted AST information
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
module CFG.PartialCFG where

import AST qualified
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Formatting
import GHC.Generics (Generic)
import Semantic qualified as SE
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL
import Types

type Label = Text

-- Basic block ID
type BBID = Int

data Condition
  = Pred {pred :: AST.Typed AST.Expr}
  | Complement
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SE.SymbolTable
  }

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: ScopeID,
    statements :: [AST.Statement]
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
    statements :: [AST.Statement]
  }
  deriving (Generic, Show)

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

initialCFGState :: ScopeID -> CFGState
initialCFGState sid =
  CFGState
    { cfg = G.empty,
      astScope = sid,
      nextBBID = 0,
      statements = []
    }

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
buildCFG (AST.ASTRoot _ _ methods) context =
  let build method =
        runState $
          flip runReaderT context $
            runExceptT $
              runCFGBuild (buildMethod method)
      updateMap map (SL.LocatedAt _ m) =
        let (r, _) = build m $ initialCFGState $ m ^. (#block . #blockID)
         in case r of
              Left e -> Left e
              Right r' -> Right $ Map.insert (m ^. (#sig . #name)) r' map
   in foldM updateMap Map.empty methods

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

appendStatement :: AST.Statement -> CFGBuild ()
appendStatement stmt = #statements %= (++ [stmt])

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
          (\(G.Node ni, nd) -> isEmptyNode nd && isSeqOut ni g)
          $ Map.assocs nodes
  let gUpdate =
        mapM_
          ( \(G.Node ni, _) -> do
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
    bridgeEdges mid inEdges (G.Node out, _) =
      forM_
        inEdges
        ( \(G.Node ni, ed) -> do
            G.deleteNode mid
            G.addEdge ni out ed
        )
    isSeqEdge SeqEdge = True
    isSeqEdge _ = False

-- CFG Builders
buildMethod :: AST.MethodDecl -> CFGBuild CFG
buildMethod method@AST.MethodDecl {sig = sig, block = block@(AST.Block _ stmts sid)} = do
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
        updateCFG (G.addEdge head id SeqEdge)
        return id
      else return stmtTail
  return (head, tail)

buildStatement :: BBID -> SL.Located AST.Statement -> CFGBuild BBID
buildStatement prev (SL.LocatedAt _ stmt@(AST.IfStmt pred ifBlock elseBlock)) = do
  appendStatement stmt
  head <- createIsolateBB
  (ifStart, ifEnd) <- buildBlock ifBlock
  (elseStart, elseEnd) <- buildBlock ifBlock
  tail <- createIsolateBB
  let gUpdate = do
        G.addEdge prev head SeqEdge
        G.addEdge head ifStart $ CondEdge $ Pred $ SL.unLocate pred
        G.addEdge head elseStart $ CondEdge Complement
        G.addEdge ifEnd tail SeqEdge
        G.addEdge elseEnd tail SeqEdge
  updateCFG gUpdate
  return tail
buildStatement prev (SL.LocatedAt _ stmt@AST.ForStmt {pred = pred, block = body}) = do
  head <- createIsolateBB
  appendStatement stmt
  predBB <- createIsolateBB
  (bodyHead, bodyTail) <- buildBlock body
  tail <- createIsolateBB
  let gUpdate = do
        G.addEdge prev head SeqEdge
        G.addEdge head predBB SeqEdge
        G.addEdge predBB bodyHead $ CondEdge $ Pred $ SL.unLocate pred
        G.addEdge predBB tail $ CondEdge Complement
        G.addEdge bodyTail predBB SeqEdge
  updateCFG gUpdate
  return tail
buildStatement prev (SL.LocatedAt _ stmt@AST.WhileStmt {pred = pred, block = body}) = do
  head <- createIsolateBB
  appendStatement stmt
  predBB <- createIsolateBB
  (bodyHead, bodyTail) <- buildBlock body
  tail <- createIsolateBB
  let gUpdate = do
        G.addEdge prev head SeqEdge
        G.addEdge head predBB SeqEdge
        G.addEdge predBB bodyHead $ CondEdge $ Pred $ SL.unLocate pred
        G.addEdge predBB tail $ CondEdge Complement
        G.addEdge bodyTail predBB SeqEdge
  updateCFG gUpdate
  return tail
buildStatement head (SL.LocatedAt _ stmt) = do
  appendStatement stmt
  return head

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
prettyPrintEdge (CondEdge (Pred AST.Typed {ele = ele})) = sformat shown ele
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: G.Graph BBID CFGNode CFGEdge -> Text
generateDotPlot G.Graph {nodes = nodes, edges = edges} =
  let preamble = "digraph G {\n"
      postamble = "}"
      nodeBoxes = Map.assocs nodes <&> (\(G.Node i, d) -> nodeBox i d)
      edgeLines =
        concatMap
          (\(G.Node from, tos) -> tos <&> \(G.Node to, d) -> edgeLine from to d)
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
