{-# LANGUAGE OverloadedStrings #-}

module CFG.GraphSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Graph (Graph)
import qualified Graph
import Test.Hspec

type NodeData = Text

type EdgeData = Text

spec :: Spec
spec = do
  describe "graph" $ do
    specEmptyGraph
    specTrivialGraph
    specCyclicGraph

specEmptyGraph :: SpecWith ()
specEmptyGraph = do
  it "construct an empty graph" $
    let emptyGraph = Graph.empty
     in (null $ Graph.nodes emptyGraph) && (null $ Graph.edges emptyGraph)

specTrivialGraph :: SpecWith ()
specTrivialGraph = do
  it "construct a trivial graph" $
    let trivialGraph = constructTrivialGraph
     in Map.size (Graph.nodes trivialGraph) == 2
  where
    constructTrivialGraph =
      let emptyGraph = Graph.empty
          (node1, graph1) = Graph.addNode "1" emptyGraph
          (node2, graph2) = Graph.addNode "2" graph1
          graph3 = Graph.addEdge node1 node2 "1-2" graph2
       in graph3

specCyclicGraph :: SpecWith ()
specCyclicGraph = do
  it "construct a cyclic graph" $
    let emptyGraph = Graph.empty
        (node1, graph1) = Graph.addNode "1" emptyGraph
        (node2, graph2) = Graph.addNode "2" graph1
        (node3, graph3) = Graph.addNode "3" graph2
        graph4 = Graph.addEdge node1 node2 "1-2" graph3
        graph5 = Graph.addEdge node2 node3 "2-3" graph4
        cyclicGraph = Graph.addEdge node3 node1 "3-1" graph5
     in Map.size (Graph.nodes cyclicGraph) == 3
          && Map.size (Graph.edges cyclicGraph) == 3
          && ( let inEdges = Graph.inBound node3 cyclicGraph
                   outEdges = Graph.outBound node3 cyclicGraph
                in length inEdges == 1
                     && getNodeData (fst (head inEdges)) cyclicGraph == Just "2"
                     && length outEdges == 1
                     && getNodeData (fst (head outEdges)) cyclicGraph == Just "1"
             )
  where
    getNodeData n g = Map.lookup n (Graph.nodes g)
