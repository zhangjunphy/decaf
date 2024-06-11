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

module CFG.Plot where

import AST qualified
import CFG.Build
import CFG.Types
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
import Data.Functor ((<&>))
import Data.GraphViz qualified as GViz
import Data.GraphViz.Attributes.Complete qualified as GViz
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Formatting
import Semantic qualified as SE
import Types
import Util.Graph qualified as G

gvizParams :: CFG -> Set (BBID, BBID) -> GViz.GraphvizParams BBID CFGNode CFGEdge () CFGNode
gvizParams (CFG _ entry exit) backEdges = GViz.nonClusteredParams {GViz.globalAttributes = globalAttrs, GViz.fmtNode = fmtNode, GViz.fmtEdge = fmtEdge}
  where
    globalAttrs = [GViz.NodeAttrs [GViz.Shape GViz.BoxShape]]
    fmtNode (bbid, node) = [GViz.Label $ GViz.StrLabel $ prettyPrintNode node]
      where
        rank
          | bbid == entry = [GViz.Rank GViz.SourceRank]
          | bbid == exit = [GViz.Rank GViz.SinkRank]
          | otherwise = [GViz.Rank GViz.SameRank]
    fmtEdge (b1, b2, edge) = [GViz.Label $ GViz.StrLabel $ prettyPrintEdge edge] ++ dir
      where
        dir
          | Set.member (b2, b1) backEdges = [GViz.Dir GViz.Back]
          | otherwise = []

cfgToDot :: CFG -> GViz.DotGraph BBID
cfgToDot cfg@(CFG graph entry exit) =
  GViz.graphElemsToDot
    (gvizParams cfg backEdges)
    (Map.toList $ graph ^. #nodes)
    reorderedEdges
  where
    edges :: [(BBID, BBID, CFGEdge)]
    edges = concatMap (\(f, t') -> t' <&> \(t, el) -> (f, t, el)) $ Map.toList $ graph ^. #edges
    backEdges :: Set (BBID, BBID)
    backEdges = Set.fromList $ filter (\(b1, b2) -> b1 > b2 && b2 /= exit) $ fmap (\(b1, b2, _) -> (b1, b2)) edges
    reorderedEdges =
      edges
        <&> \(b1, b2, e) -> if Set.member (b1, b2) backEdges then (b2, b1, e) else (b1, b2, e)

prettyPrintNode :: CFGNode -> LT.Text
prettyPrintNode CFGNode {bb = BasicBlock {bbid = id, statements = stmts}} =
  let idText = [format ("<id:" %+ int % ">") id]
      segments = stmts <&> format shown
   in LT.intercalate "\n" $ idText ++ segments

prettyPrintEdge :: CFGEdge -> LT.Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = format shown var
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: CFG -> Text
generateDotPlot cfg = LT.toStrict $ GViz.printDotGraph $ cfgToDot cfg

plot :: AST.ASTRoot -> SE.SemanticInfo -> Either [String] String
plot root si =
  let context = CFGContext si
      res = buildCFG root context
   in case res of
        Left (CFGExcept msg) -> Left [Text.unpack msg]
        Right cfgs -> Right $ Text.unpack $ mconcat $ Map.elems cfgs <&> generateDotPlot
