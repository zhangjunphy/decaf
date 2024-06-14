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
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3, _Just)
import Data.Functor ((<&>))
import Data.GraphViz (GraphvizParams (isDotCluster))
import Data.GraphViz qualified as GViz
import Data.GraphViz.Attributes.Complete qualified as GViz
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Formatting
import Semantic qualified as SE
import Types
import Util.Graph qualified as G

-- Reorder some backward edges introduced by loops so graphviz could find a
-- clear ordering of the nodes.
findBackEdges :: SingleFileCFG -> Set (BBID, BBID)
findBackEdges file =
  Set.unions $ fmap (findBackEdgesInCFG . snd) (Map.toList $ file ^. #cfgs)
  where
    findBackEdgesInCFG (CFG g _ exit _) =
      Set.fromList $
        filter (\(b1, b2) -> b1 > b2 && b2 /= exit) $
          fmap (\(b1, b2, _) -> (b1, b2)) (G.edgeToList g)

nodeToCFG :: SingleFileCFG -> Map BBID Name
nodeToCFG (SingleFileCFG _ cfgs) =
  Map.fromList $
    concatMap
      (\(n, cfg) -> G.nodeToList (cfg ^. #graph) <&> \(bbid, _) -> (bbid, n))
      (Map.toList cfgs)

findCFG :: SingleFileCFG -> Map BBID Name -> BBID -> Maybe CFG
findCFG file nodeMap bbid =
    Map.lookup bbid nodeMap >>= \name -> Map.lookup name (file ^. #cfgs)

fmtNode :: SingleFileCFG -> (BBID, BasicBlock) -> [GViz.Attribute]
fmtNode file (bbid, bb) =
  let nodeMap = nodeToCFG file
      cfg = findCFG file nodeMap bbid
   in [GViz.Label $ GViz.StrLabel $ prettyPrintNode bbid bb cfg]

fmtEdge :: SingleFileCFG -> (BBID, BBID, CFGEdge) -> [GViz.Attribute]
fmtEdge file (src, dst, edge) =
  [GViz.Label $ GViz.StrLabel $ prettyPrintEdge edge] ++ dir
  where
    nodeMap = nodeToCFG file
    backEdges = findBackEdges file
    cfg = findCFG file nodeMap src
    -- Some of the nodes had their src->dst reversed in cfgToDot.
    -- We need to mark them to get a correct arrow.
    dir
      | Set.member (dst, src) backEdges = [GViz.Dir GViz.Back]
      | otherwise = []

clusterByCFG :: SingleFileCFG -> (BBID, BasicBlock) -> GViz.NodeCluster Name (BBID, BasicBlock)
clusterByCFG file n@(bbid, _) = case cfgName of
  Nothing -> GViz.N n
  Just name -> GViz.C name (GViz.N n)
  where
    nodeMap = nodeToCFG file
    cfgName = findCFG file nodeMap bbid <&> view (#sig . #name)

gvizParams :: SingleFileCFG -> GViz.GraphvizParams BBID BasicBlock CFGEdge Name BasicBlock
gvizParams file =
  GViz.defaultParams
    { GViz.isDirected = True,
      GViz.globalAttributes = globalAttrs,
      GViz.clusterBy = clusterByCFG file,
      GViz.isDotCluster = const False,
      GViz.clusterID = GViz.Str . LT.fromStrict,
      GViz.fmtNode = fmtNode file,
      GViz.fmtEdge = fmtEdge file
    }
  where
    globalAttrs = [GViz.NodeAttrs [GViz.Shape GViz.BoxShape]]

cfgToDot :: SingleFileCFG -> GViz.DotGraph BBID
cfgToDot file =
  GViz.graphElemsToDot (gvizParams file) nodes reorderedEdges
  where
    graphList = view #graph <$> Map.elems (file ^. #cfgs)
    nodes = [(0, file ^. #global)] ++ concatMap G.nodeToList graphList
    edges = concatMap G.edgeToList graphList
    backEdges = findBackEdges file
    reorderedEdges =
      edges
        <&> \(b1, b2, e) ->
          if Set.member (b1, b2) backEdges
            then (b2, b1, e)
            else (b1, b2, e)

prettyPrintNode :: BBID -> BasicBlock -> Maybe CFG -> LT.Text
prettyPrintNode bbid BasicBlock {bbid = id, statements = stmts} (Just (CFG _ entry exit sig)) =
  let idText = [format ("<id:" %+ int %+ stext % ">") id entryExit]
      segments = stmts <&> format shown
   in LT.intercalate "\n" $ idText ++ segments
  where
    entryExit
      | bbid == entry = sformat ("[entry(" % stext % ")]") (AST.mangle sig)
      | bbid == exit = "[exit]"
      | otherwise = ""
prettyPrintNode _ BasicBlock {bbid = id, statements = stmts} Nothing =
  let idText = [format ("<id:" %+ int %+ stext % ">") id "[global]"]
      segments = stmts <&> format shown
   in LT.intercalate "\n" $ idText ++ segments

prettyPrintEdge :: CFGEdge -> LT.Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = format shown var
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: SingleFileCFG -> Text
generateDotPlot file = LT.toStrict $ GViz.printDotGraph $ cfgToDot file
