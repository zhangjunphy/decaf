{-# LANGUAGE OverloadedStrings #-}

module CFG.CFGSpec where

import Test.Hspec

import Data.Text (Text)

import CFG (CFG)
import qualified CFG
import qualified Data.Map.Strict as Map

newtype NodeData = NodeData Text
newtype EdgeData = EdgeData Text

spec :: Spec
spec = do 
  describe "test cfg construction" $ do
    specEmptyCFG
    specTrivialCFG

constructTrivialCFG :: CFG NodeData EdgeData
constructTrivialCFG =
  let emptyCFG = CFG.empty
      (node1, cfg1) = CFG.addNode (NodeData "1") emptyCFG
      (node2, cfg2) = CFG.addNode (NodeData "2") cfg1
      cfg3 = CFG.addEdge node1 node2 (EdgeData "1-2") cfg2
   in cfg3

specEmptyCFG :: SpecWith ()
specEmptyCFG = do
    it "should construct an empty cfg" $
      let emptyCFG = CFG.empty
      in (null $ CFG.nodes emptyCFG) && (null $ CFG.edges emptyCFG)

specTrivialCFG :: SpecWith ()
specTrivialCFG = do
    it "should construct a trivial cfg" $
      let trivialCFG = constructTrivialCFG
      in Map.size (CFG.nodes trivialCFG) == 2
