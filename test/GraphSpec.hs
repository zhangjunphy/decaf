{-# LANGUAGE OverloadedStrings #-}

module GraphSpec where

import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import Util.Graph (Graph)
import Util.Graph qualified as Graph

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
      n1 <- Graph.addNode "1" "d1"
      n2 <- Graph.addNode "2" "d2"
      Graph.addEdge n1 n2 "1-2"

specCyclicGraph :: SpecWith ()
specCyclicGraph = do
  it "construct a cyclic graph" $
    let constructG = do
          n1 <- Graph.addNode "1" "d1"
          n2 <- Graph.addNode "2" "d2"
          n3 <- Graph.addNode "3" "d3"
          Graph.addEdge n1 n2 "1-2"
          Graph.addEdge n2 n3 "2-3"
          Graph.addEdge n3 n1 "3-1"
        (Right cyclicGraph) = Graph.build constructG
     in Map.size (Graph.nodes cyclicGraph) == 3
          && Map.size (Graph.edges cyclicGraph) == 3
          && ( let inEdges = Graph.inBound "3" cyclicGraph
                   outEdges = Graph.outBound "3" cyclicGraph
                in length inEdges == 1
                     && getNodeData (fst (head inEdges)) cyclicGraph == Just "d2"
                     && length outEdges == 1
                     && getNodeData (fst (head outEdges)) cyclicGraph == Just "d1"
             )
  where
    getNodeData n g = Map.lookup n (Graph.nodes g)

newtype TraverseMonad ni a = TraverseMonad
  {runTraverse :: State [ni] a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState [ni]
    )

sortedList :: (Ord ni) => Graph ni nd ed -> [ni]
sortedList g =
  let (_, s) = (runState $ runTraverse (Graph.traverseM_ (\ni _ -> modify' (\l -> l ++ [ni])) g)) []
   in s

traverseGraph :: SpecWith ()
traverseGraph = do
  it "traverse graph in topological order" $
    let constructG = do
          n1 <- Graph.addNode "1" "d1"
          n2 <- Graph.addNode "2" "d2"
          n3 <- Graph.addNode "3" "d3"
          n4 <- Graph.addNode "4" "d4"
          Graph.addEdge n4 n2 "4-2"
          Graph.addEdge n4 n3 "4-3"
          Graph.addEdge n2 n1 "2-1"
          Graph.addEdge n3 n1 "3-1"
        (Right graph) = Graph.build constructG
        lst = sortedList graph
     in head lst == "4" && last lst == "1"
