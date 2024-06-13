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

-- Graph -- A graph implementation to help build CFG
-- Supports cyclical graphs. Also allows dynamic updates to the graph for
-- transformations over CFG.
-- TODO: Perhaps we could just use FGL/ALGA?

module Util.Graph
  ( empty
  , Graph(..) 
  , outBound
  , inBound
  , lookupNode
  , updateNodeWith
  , updateNode
  , union
  , GraphBuilder(..)
  , addNode
  , addEdge
  , deleteNode
  , deleteEdge
  , update
  , build
  , strictlyDominate
  , strictlyPostDominate
  , topologicalTraverse
  ) where

import Control.Lens ((%=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Monoid
import Data.Functor
import Data.Generics.Labels
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as Text
import GHC.Generics (Generic)

data Graph ni nd ed = Graph
  { nodes :: !(Map ni nd),
    edges :: !(Map (ni, ni) ed)
  }
  deriving (Show, Generic)

type GraphException = Text

empty :: Graph ni nd ed
empty = Graph Map.empty Map.empty

flattenEdgeTuple :: ((ni, ni), d) -> (ni, ni, d)
flattenEdgeTuple ((src, dst), d) = (src, dst, d)

outBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(ni, ni, ed)]
outBound idx g = fmap flattenEdgeTuple $
  Map.toList $ Map.filterWithKey (\(src, _) _ -> src == idx) (edges g)

inBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(ni, ni, ed)]
inBound idx g = fmap flattenEdgeTuple $
  Map.toList $ Map.filterWithKey (\(_, dst) _ -> dst == idx) (edges g)

lookupNode :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Maybe nd
lookupNode nid g = Map.lookup nid $ nodes g

union :: (Eq ni, Ord ni) => Graph ni nd ed -> Graph ni nd ed -> Graph ni nd ed
union g1 g2 =
  Graph
    { nodes = nodes g1 `Map.union` nodes g2,
      edges = edges g1 `Map.union` edges g2
    }

newtype GraphBuilder ni nd ed a = GraphBuilder
  { buildGraph :: (ExceptT GraphException (State (Graph ni nd ed))) a }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError GraphException,
      MonadState (Graph ni nd ed)
    )

addNode :: (Eq ni, Ord ni) => ni -> nd -> GraphBuilder ni nd ed ()
addNode idx dt = do
  modify $ \g -> g {nodes = Map.insert idx dt (nodes g)}

addEdge :: (Eq ni, Ord ni) => ni -> ni -> ed -> GraphBuilder ni nd ed ()
addEdge src dst dt = do
  modify $ \g -> g {edges = Map.insert (src, dst) dt (edges g)}

deleteNode :: (Eq ni, Ord ni) => ni -> GraphBuilder ni nd ed ()
deleteNode n = do
  g <- get
  let nodes' = Map.delete n (nodes g)
      edges' = Map.filterWithKey (\(s, d) _ -> s /= n && d /= n) (edges g)
  modify $ \g -> g {nodes = nodes', edges = edges'}

deleteEdge :: (Eq ni, Ord ni) => ni -> ni -> GraphBuilder ni nd ed ()
deleteEdge src dst = do
  g <- get
  let edges' = Map.delete (src, dst) (edges g)
  modify $ \g -> g {edges = edges'}

updateNodeWith :: (Eq ni, Ord ni) => ni -> (Maybe nd -> nd) -> GraphBuilder ni nd ed ()
updateNodeWith nid f = do
  g <- get
  let nds = nodes g
  let nd = lookupNode nid g
  let nds' = Map.alter (const $ Just $ f nd) nid nds
  modify $ \g -> g {nodes = nds'}

updateNode :: (Eq ni, Ord ni) => ni -> nd -> GraphBuilder ni nd ed ()
updateNode nid d = do
  g <- get
  let nds = nodes g
  let nds' = Map.alter (const $ Just d) nid nds
  modify $ \g -> g {nodes = nds'}

updateEdge :: (Eq ni, Ord ni) => ni -> ni -> ed -> GraphBuilder ni nd ed ()
updateEdge src dst d = do
  deleteEdge src dst
  addEdge src dst d

update :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Graph ni nd ed -> Either Text (Graph ni nd ed)
update bd init =
  let (except, g) = (runState $ runExceptT $ buildGraph bd) init
   in case except of
        Left except -> Left except
        Right _ -> Right g

build :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Either Text (Graph ni nd ed)
build bd = update bd empty

topologicalTraverse :: (Eq ni, Ord ni, Monoid (f a)) => (ni -> nd -> f a) -> Graph ni nd ed -> f a
topologicalTraverse f g@Graph {nodes = nodes} = recurse initIndegree g
  where
    initIndegree = Map.mapWithKey (\i d -> length $ inBound i g) nodes
    findZeroIndegree :: Map ni Int -> [ni]
    findZeroIndegree m = fmap fst $ Map.toList $ Map.filter (== 0) m
    updateIndegree i m g =
      let m' = List.foldl' (\m (_, ni, _) -> Map.adjust (\x -> x - 1) ni m) m $ outBound i g
          m'' = Map.delete i m'
       in m''
    recurse indegree g =
      let zeroIndegree = findZeroIndegree indegree
      in case zeroIndegree of
        [] -> mempty
        (n : ns) ->
          let ele = f n (Maybe.fromJust $ lookupNode n g)
              indegree' = updateIndegree n indegree g
          in ele <> recurse indegree' g

newtype Memoize ni m a = Memoize
  { unmem :: State m a
  }
  deriving (Functor, Applicative, Monad, MonadState m)

data Memory ni = Memory
  { processing :: !(Set ni),
    finished :: !(Map ni (Set ni))
  }
  deriving (Generic)

recurse_ :: (Eq ni, Ord ni) => (ni -> Graph ni nd ed -> [ni]) -> ni -> Graph ni nd ed -> Memoize ni (Memory ni) (Set ni)
recurse_ f idx g = do
  (Memory processing finished) <- get
  #processing %= Set.insert idx
  case Map.lookup idx finished of
    Just res -> return res
    Nothing -> do
      let direct = f idx g
      -- Avoid infinite loops by ignoring nodes being processed in the stack
      indirect <- mapM (\i -> recurse_ f i g) (filter (`Set.notMember` processing) direct)
      let res = Set.union (Set.fromList direct) (Set.unions indirect)
      #finished %= Map.insert idx res
      return res

strictlyDominate :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Set ni
strictlyDominate idx g =
  evalState (unmem $ recurse_ (\i g -> outBound i g <&> \(_, dst, _) -> dst) idx g) (Memory Set.empty Map.empty)

strictlyPostDominate :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Set ni
strictlyPostDominate idx g =
  evalState (unmem $ recurse_ (\i g -> inBound i g <&> \(src, _, _) -> src) idx g) (Memory Set.empty Map.empty)
