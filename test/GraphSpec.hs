{-# LANGUAGE OverloadedStrings #-}

module GraphSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Graph
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
    let (Right trivialGraph) = build constructTrivialGraph
     in Map.size (Graph.nodes trivialGraph) == 2
  where
    constructTrivialGraph = do
      n1 <- addNode "1"
      n2 <- addNode "2"
      addEdge n1 n2 "1-2"

specCyclicGraph :: SpecWith ()
specCyclicGraph = do
  it "construct a cyclic graph" $
    let constructG = do
          n1 <- addNode "1"
          n2 <- addNode "2"
          n3 <- addNode "3"
          addEdge n1 n2 "1-2"
          addEdge n2 n3 "2-3"
          addEdge n3 n1 "3-1"
        (Right cyclicGraph) = build constructG
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
