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

module GraphSpec where

import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import Util.Graph (Graph)
import Util.Graph qualified as Graph
import Data.Functor ((<&>))

type NodeData = Text

type EdgeData = Text

spec :: Spec
spec = do
  describe "graph" $ do
    specEmptyGraph
    specTrivialGraph
    specCyclicGraph
    traverseGraph

specEmptyGraph :: SpecWith ()
specEmptyGraph = do
  it "construct an empty graph" $
    let emptyGraph = Graph.empty
     in (null $ Graph.nodes emptyGraph) && (null $ Graph.edges emptyGraph)

specTrivialGraph :: SpecWith ()
specTrivialGraph = do
  it "construct a trivial graph" $
    let (Right trivialGraph) = Graph.build constructTrivialGraph
     in Map.size (Graph.nodes trivialGraph) == 2
  where
    constructTrivialGraph = do
      Graph.addNode 1 "d1"
      Graph.addNode 2 "d2"
      Graph.addEdge 1 2 "1-2"

specCyclicGraph :: SpecWith ()
specCyclicGraph = do
  it "construct a cyclic graph" $
    let constructG = do
          Graph.addNode 1 "d1"
          Graph.addNode 2 "d2"
          Graph.addNode 3 "d3"
          Graph.addEdge 1 2 "1-2"
          Graph.addEdge 2 3 "2-3"
          Graph.addEdge 3 1 "3-1"
        (Right cyclicGraph) = Graph.build constructG
     in Map.size (Graph.nodes cyclicGraph) == 3
          && Map.size (Graph.edges cyclicGraph) == 3
          && ( let inEdges = Graph.inBound 3 cyclicGraph
                   outEdges = Graph.outBound 3 cyclicGraph
                in length inEdges == 1
                     && getNodeData ((\(src, _, _) -> src) (head inEdges) ) cyclicGraph == Just "d2"
                     && length outEdges == 1
                     && getNodeData ((\(_, dst, _) -> dst) (head outEdges) ) cyclicGraph == Just "d1"
             )
  where
    getNodeData n g = Map.lookup n (Graph.nodes g)

topologicalSort :: (Ord ni) => Graph ni nd ed -> [ni]
topologicalSort = Graph.topologicalTraverse (\ni _ -> [ni]) 

traverseGraph :: SpecWith ()
traverseGraph = do
  it "traverse graph in topological order" $
    let constructG = do
          Graph.addNode 1 "d1"
          Graph.addNode 2 "d2"
          Graph.addNode 3 "d3"
          Graph.addNode 4 "d4"
          Graph.addEdge 4 2 "4-2"
          Graph.addEdge 4 3 "4-3"
          Graph.addEdge 2 1 "2-1"
          Graph.addEdge 3 1 "3-1"
        (Right graph) = Graph.build constructG
        lst = topologicalSort graph
     in head lst == 4 && last lst == 1
