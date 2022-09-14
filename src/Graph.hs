-- Graph -- A graph implementation to help build CFG
-- Copyright (C) 2018 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import Control.Monad
import Data.Functor
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Node = Node Int
  deriving (Eq, Ord, Show)

data Graph nd ed = Graph
  { nodes :: Map Node nd,
    edges :: Map Node [(Node, ed)],
    start :: Maybe Node,
    nextNodeIdx :: Int
  }
  deriving (Show)

empty :: Graph nd ed
empty = Graph Map.empty Map.empty Nothing 0

addNode :: nd -> Graph nd ed -> (Node, Graph nd ed)
addNode dt cfg =
  let n = Node (nextNodeIdx cfg)
   in (n, cfg {nodes = Map.insert n dt (nodes cfg), nextNodeIdx = nextNodeIdx cfg + 1})

addEdge :: Node -> Node -> ed -> Graph nd ed -> Graph nd ed
addEdge start end dt cfg =
  let edges' = Map.insertWith (++) start [(end, dt)] (edges cfg)
   in cfg {edges = edges'}

deleteEdge :: Node -> Node -> Graph nd ed -> Graph nd ed
deleteEdge start end cfg =
  let edges' = Map.update (Just <$> filter (\(e, dt) -> e /= end)) start (edges cfg)
   in cfg {edges = edges'}

deleteNode :: Node -> Graph nd ed -> Graph nd ed
deleteNode node cfg =
  let nodes' = Map.delete node (nodes cfg)
      edges' = Map.filterWithKey (\k v -> k /= node) (edges cfg)
      edges'' = Map.mapMaybe (Just <$> filter (\(end, dt) -> end /= node)) edges'
   in cfg {nodes = nodes'}

outBound :: Node -> Graph nd ed -> [(Node, ed)]
outBound node cfg = concat $ Map.lookup node (edges cfg)

inBound :: Node -> Graph nd ed -> [(Node, ed)]
inBound node cfg =
  let edges' = Map.mapMaybe (Just <$> filter (\(end, _) -> end == node)) (edges cfg)
      nodes' =
        concat (Map.assocs edges' <&> \(src, dsts) -> zip (repeat src) dsts)
          <&> \(src, (_, ed)) -> (src, ed)
   in nodes'
