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
module Util.Graph where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Functor
import Data.Map (Map, mapWithKey)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as Text
import Data.Graph.Inductive.Graph qualified as FGL

newtype Node a = Node a
  deriving (Eq, Ord, Show)

data Graph ni nd ed = Graph
  { nodes :: Map (Node ni) nd,
    edges :: Map (Node ni) [(Node ni, ed)]
  }
  deriving (Show)

type GraphException = Text

empty :: Graph ni nd ed
empty = Graph Map.empty Map.empty

outBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(Node ni, ed)]
outBound idx g = concat $ Map.lookup (Node idx) (edges g)

inBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(Node ni, ed)]
inBound idx cfg =
  let edges' = Map.mapMaybe (Just <$> filter (\(end, _) -> end == Node idx)) (edges cfg)
      nodes' =
        concat (Map.assocs edges' <&> \(src, dsts) -> zip (repeat src) dsts)
          <&> \(src, (_, ed)) -> (src, ed)
   in nodes'

lookupNode :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Maybe nd
lookupNode nid g = Map.lookup (Node nid) $ nodes g

updateNodeWith :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> (Maybe nd -> nd) -> Graph ni nd ed
updateNodeWith nid g f =
  let nds = nodes g
      nd = lookupNode nid g
      nds' = Map.alter (const $ Just $ f nd) (Node nid) nds
   in g {nodes = nds'}

updateNode :: (Eq ni, Ord ni) => ni -> nd -> Graph ni nd ed -> Graph ni nd ed
updateNode nid d g =
  let nds = nodes g
      nds' = Map.alter (const $ Just d) (Node nid) nds
   in g {nodes = nds'}

union :: (Eq ni, Ord ni) => Graph ni nd ed -> Graph ni nd ed -> Graph ni nd ed
union g1 g2 =
  Graph
    { nodes = nodes g1 `Map.union` nodes g2,
      edges = edges g1 `Map.union` edges g2
    }

newtype GraphBuilder ni nd ed a = GraphBuilder
  {buildGraph :: (ExceptT GraphException (State (Graph ni nd ed))) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError GraphException,
      MonadState (Graph ni nd ed)
    )

addNode :: (Eq ni, Ord ni) => ni -> nd -> GraphBuilder ni nd ed ni
addNode idx dt = do
  let n = Node idx
  modify $ \g -> g {nodes = Map.insert n dt (nodes g)}
  return idx

addEdge :: (Eq ni, Ord ni) => ni -> ni -> ed -> GraphBuilder ni nd ed ()
addEdge start end dt = do
  g <- get
  let edges' = Map.insertWith (++) (Node start) [(Node end, dt)] (edges g)
  modify $ \g -> g {edges = edges'}

deleteNode :: (Eq ni, Ord ni) => ni -> GraphBuilder ni nd ed ()
deleteNode n = do
  g <- get
  let nodes' = Map.delete (Node n) (nodes g)
      edges' = Map.filterWithKey (\k v -> k /= Node n) (edges g)
      edges'' = Map.mapMaybe (Just <$> filter (\(end, dt) -> end /= Node n)) edges'
  modify $ \g -> g {nodes = nodes', edges = edges''}

deleteEdge :: (Eq ni, Ord ni) => ni -> ni -> GraphBuilder ni nd ed ()
deleteEdge start end = do
  g <- get
  let edges' = Map.update (Just <$> filter (\(e, dt) -> e /= Node end)) (Node start) (edges g)
  modify $ \g -> g {edges = edges'}

update :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Graph ni nd ed -> Either Text (Graph ni nd ed)
update bd init =
  let (except, g) = (runState $ runExceptT $ buildGraph bd) init
   in case except of
        Left except -> Left except
        Right _ -> Right g

build :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Either Text (Graph ni nd ed)
build bd = update bd empty
