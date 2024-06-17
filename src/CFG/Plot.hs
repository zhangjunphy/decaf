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
findBackEdges :: CFG -> Set (BBID, BBID)
findBackEdges (CFG g _ exit _) =
  Set.fromList $
    filter (\(b1, b2) -> b1 > b2 && b2 /= exit) $
      fmap (\(b1, b2, _) -> (b1, b2)) (G.edgeToList g)

data GVizNode = GVizNode
  { id :: !BBID,
    label :: !Text
  }

instance Show GVizNode where
  show (GVizNode id label) = formatToString (int %+ "[label=\"" % stext % "\"]") id label

data GVizEdge = GVizEdge
  { from :: !BBID,
    to :: !BBID,
    label :: !Text,
    dir :: !Text
  }

instance Show GVizEdge where
  show (GVizEdge from to label dir) =
    formatToString
      (int %+ "->" %+ int %+ "[label=\"" % stext % "\", dir=" % stext % "]")
      from
      to
      label
      dir

data GVizSubgraph = GVizSubgraph
  { name :: !Text,
    nodes :: ![GVizNode],
    edges :: ![GVizEdge]
  }

instance Show GVizSubgraph where
  show (GVizSubgraph name nodes edges) =
    formatToString
      ( "subgraph "
          %+ stext
          %+ "{\n"
          % concatenated (shown % ";\n")
          % concatenated (shown % ";\n")
          % "}"
      )
      name
      nodes
      edges

data GVizGraph = GVizGraph
  { subgraphs :: ![GVizSubgraph],
    nodes :: ![GVizNode],
    edges :: ![GVizEdge]
  }

instance Show GVizGraph where
  show (GVizGraph subgraphs nodes edges) =
    formatToString
      ( "digraph {\n"
          % "node [shape=box];\n"
          % concatenated (shown % "\n")
          % concatenated (shown % ";\n")
          % concatenated (shown % ";\n")
          % "}"
      )
      subgraphs
      nodes
      edges

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

basicBlockToNode :: BasicBlock -> GVizNode
basicBlockToNode BasicBlock {bbid = id, statements = ssas} =
  GVizNode id $ Text.intercalate "\n" (ssas <&> (Text.pack . show))

cfgToSubgraph :: CFG -> GVizSubgraph
cfgToSubgraph cfg = GVizSubgraph name nodes edges
  where
    name = AST.mangle (cfg ^. #sig)
    graph = cfg ^. #graph
    nodes = G.nodeToList graph <&> basicBlockToNode . snd
    backEdges = findBackEdges cfg
    isBackEdge (from, to, _) = Set.member (from, to) backEdges
    convertEdge edge@(from, to, _) =
      GVizEdge from to "" (if isBackEdge edge then "back" else "forward")
    edges = G.edgeToList graph <&> convertEdge

fileCFGsToGraph :: SingleFileCFG -> GVizGraph
fileCFGsToGraph (SingleFileCFG global cfgs) = GVizGraph subgraphs [globalNode] []
  where
    globalNode = basicBlockToNode global
    subgraphs = Map.elems cfgs <&> cfgToSubgraph
