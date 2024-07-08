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

module CFG.Plot (fileCFGsToDot) where

import AST qualified
import CFG.Build
import CFG.Types
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3, _Just)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Formatting
import SSA qualified
import Semantic qualified as SE
import Types
import Util.Graph qualified as G

-- Reorder some backward edges introduced by loops so graphviz could find a
-- clear ordering of the nodes.
findBackEdges :: CFG -> Set (BBID, BBID)
findBackEdges (CFG g _ _ _ _) =
  Set.fromList $
    filter (uncurry (>)) $
      fmap (\(b1, b2, _) -> (b1, b2)) (G.edgeToList g)

data GVizNode = GVizNode
  { id :: !BBID,
    label :: !Text,
    rank :: !(Maybe Text)
  }

instance Show GVizNode where
  show (GVizNode id label (Just rank)) = formatToString (int %+ "[label=\"" % stext % "\", rank=\"" % stext % "\"]") id label rank
  show (GVizNode id label Nothing) = formatToString (int %+ "[label=\"" % stext % "\"]") id label

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
      ( "subgraph"
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
          % concatenated (shown % ";\n")
          % concatenated (shown % ";\n")
          % concatenated (shown % "\n")
          % "}"
      )
      nodes
      edges
      subgraphs

basicBlockToNode :: Maybe CFG -> BasicBlock -> GVizNode
basicBlockToNode (Just (CFG _ entry exit args sig)) BasicBlock {bbid = id, statements = stmts} =
  let idText = [sformat ("<id:" %+ int %+ stext % ">") id entryExit]
      segments = stmts <&> sformat shown
   in GVizNode id (Text.intercalate "\\n" $ idText ++ segments) (Just "same")
  where
    ppVarWithType :: Format r (SSA.Var -> r)
    ppVarWithType = viewed #tpe shown <%+> shown
    methodAndArgs = sformat (stext % "(" % intercalated ", " ppVarWithType % ")") (AST.mangle sig) args
    entryExit
      | id == entry = sformat ("[entry(" % stext % ")]") methodAndArgs
      | id == exit = "[exit]"
      | otherwise = ""
basicBlockToNode Nothing BasicBlock {bbid = id, statements = stmts} =
  let idText = [sformat ("<id:" %+ int %+ stext % ">") id "[global]"]
      segments = stmts <&> sformat shown
   in GVizNode id (Text.intercalate "\\n" $ idText ++ segments) (Just "source")

prettyPrintEdge :: CFGEdge -> Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = sformat shown var
prettyPrintEdge (CondEdge (Complement var)) = sformat ("!" % shown) var

cfgToSubgraph :: CFG -> GVizSubgraph
cfgToSubgraph cfg = GVizSubgraph name nodes edges
  where
    name = cfg ^. #sig . #name
    graph = cfg ^. #graph
    nodes = G.nodeToList graph <&> basicBlockToNode (Just cfg) . snd
    backEdges = findBackEdges cfg
    isBackEdge (from, to, _) = Set.member (from, to) backEdges
    convertEdge edge@(from, to, ed) =
      let (from', to', dir) =
            if isBackEdge edge
              then (to, from, "back")
              else (from, to, "forward")
       in GVizEdge from' to' (prettyPrintEdge ed) dir
    edges = G.edgeToList graph <&> convertEdge

fileCFGsToGraph :: SingleFileCFG -> GVizGraph
fileCFGsToGraph (SingleFileCFG global cfgs) = GVizGraph subgraphs [globalNode] []
  where
    globalNode = basicBlockToNode Nothing global
    subgraphs = Map.elems cfgs <&> cfgToSubgraph

fileCFGsToDot :: SingleFileCFG -> Text
fileCFGsToDot = Text.pack . show . fileCFGsToGraph
