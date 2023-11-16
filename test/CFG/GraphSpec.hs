{-# LANGUAGE OverloadedStrings #-}

module CFG.GraphSpec where

import Test.Hspec

import Data.Text (Text)

import Graph (Graph)
import qualified Graph
import qualified Data.Map.Strict as Map

newtype NodeData = NodeData Text
newtype EdgeData = EdgeData Text

spec :: Spec
spec = do 
  describe "test graph construction" $ do
    specEmptyGraph
    specTrivialGraph

constructTrivialGraph :: Graph NodeData EdgeData
constructTrivialGraph =
  let emptyGraph = Graph.empty
      (node1, graph1) = Graph.addNode (NodeData "1") emptyGraph
      (node2, graph2) = Graph.addNode (NodeData "2") graph1
      graph3 = Graph.addEdge node1 node2 (EdgeData "1-2") graph2
   in graph3

specEmptyGraph :: SpecWith ()
specEmptyGraph = do
    it "should construct an empty graph" $
      let emptyGraph = Graph.empty
      in (null $ Graph.nodes emptyGraph) && (null $ Graph.edges emptyGraph)

specTrivialGraph :: SpecWith ()
specTrivialGraph = do
    it "should construct a trivial graph" $
      let trivialGraph = constructTrivialGraph
      in Map.size (Graph.nodes trivialGraph) == 2
