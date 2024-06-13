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

module CFG.Plot (generateDotPlot) where

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

edges :: CFG -> [(BBID, BBID, CFGEdge)]
edges (CFG graph _ _) = fmap (\((src, dst), d) -> (src, dst, d)) $ Map.toList $ graph ^. #edges

-- Reorder some backward edges introduced by loops so graphviz could find a
-- clear ordering of the nodes.
backEdges :: CFG -> Set (BBID, BBID)
backEdges cfg@(CFG _ _ exit) = Set.fromList $ filter (\(b1, b2) -> b1 > b2 && b2 /= exit) $ fmap (\(b1, b2, _) -> (b1, b2)) (edges cfg)

gvizParams :: CFG -> GViz.GraphvizParams BBID CFGNode CFGEdge () CFGNode
gvizParams cfg@(CFG _ entry exit) =
  GViz.nonClusteredParams
    { GViz.globalAttributes = globalAttrs,
      GViz.fmtNode = fmtNode,
      GViz.fmtEdge = fmtEdge
    }
  where
    backEdges' = backEdges cfg
    globalAttrs = [GViz.NodeAttrs [GViz.Shape GViz.BoxShape]]
    fmtNode (bbid, node) = [GViz.Label $ GViz.StrLabel $ prettyPrintNode bbid node cfg]
    fmtEdge (b1, b2, edge) = [GViz.Label $ GViz.StrLabel $ prettyPrintEdge edge] ++ dir
      where
        -- Some of the nodes had their src->dst reversed in cfgToDot.
        -- We need to mark them to get a correct arrow.
        dir
          | Set.member (b2, b1) backEdges' = [GViz.Dir GViz.Back]
          | otherwise = []

cfgToDot :: CFG -> GViz.DotGraph BBID
cfgToDot cfg@(CFG graph entry exit) =
  GViz.graphElemsToDot
    (gvizParams cfg)
    (Map.toList $ graph ^. #nodes)
    reorderedEdges
  where
    reorderedEdges =
      edges cfg
        <&> \(b1, b2, e) ->
          if Set.member (b1, b2) (backEdges cfg)
            then (b2, b1, e)
            else (b1, b2, e)

prettyPrintNode :: BBID -> CFGNode -> CFG -> LT.Text
prettyPrintNode bbid CFGNode {bb = BasicBlock {bbid = id, statements = stmts}} (CFG _ entry exit) =
  let idText = [format ("<id:" %+ int %+ stext % ">") id entryExit]
      segments = stmts <&> format shown
   in LT.intercalate "\n" $ idText ++ segments
  where
    entryExit
      | bbid == entry = "[entry]"
      | bbid == exit = "[exit]"
      | otherwise = ""

prettyPrintEdge :: CFGEdge -> LT.Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = format shown var
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: CFG -> Text
generateDotPlot cfg = LT.toStrict $ GViz.printDotGraph $ cfgToDot cfg
