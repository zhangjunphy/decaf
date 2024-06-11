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
  ) where

import Control.Lens ((%=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
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
    edges :: !(Map ni [(ni, ed)])
  }
  deriving (Show)

type GraphException = Text

empty :: Graph ni nd ed
empty = Graph Map.empty Map.empty

outBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(ni, ed)]
outBound idx g = concat $ Map.lookup idx (edges g)

inBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(ni, ed)]
inBound idx cfg =
  let edges' = Map.mapMaybe (Just <$> filter (\(end, _) -> end == idx)) (edges cfg)
      nodes' =
        concat (Map.assocs edges' <&> \(src, dsts) -> zip (repeat src) dsts)
          <&> \(src, (_, ed)) -> (src, ed)
   in nodes'

lookupNode :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Maybe nd
lookupNode nid g = Map.lookup nid $ nodes g

updateNodeWith :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> (Maybe nd -> nd) -> Graph ni nd ed
updateNodeWith nid g f =
  let nds = nodes g
      nd = lookupNode nid g
      nds' = Map.alter (const $ Just $ f nd) nid nds
   in g {nodes = nds'}

updateNode :: (Eq ni, Ord ni) => ni -> nd -> Graph ni nd ed -> Graph ni nd ed
updateNode nid d g =
  let nds = nodes g
      nds' = Map.alter (const $ Just d) nid nds
   in g {nodes = nds'}

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

addNode :: (Eq ni, Ord ni) => ni -> nd -> GraphBuilder ni nd ed ni
addNode idx dt = do
  modify $ \g -> g {nodes = Map.insert idx dt (nodes g)}
  return idx

addEdge :: (Eq ni, Ord ni) => ni -> ni -> ed -> GraphBuilder ni nd ed ()
addEdge start end dt = do
  g <- get
  let edges' = Map.insertWith (++) start [(end, dt)] (edges g)
  modify $ \g -> g {edges = edges'}

deleteNode :: (Eq ni, Ord ni) => ni -> GraphBuilder ni nd ed ()
deleteNode n = do
  g <- get
  let nodes' = Map.delete n (nodes g)
      edges' = Map.filterWithKey (\k v -> k /= n) (edges g)
      edges'' = Map.mapMaybe (Just <$> filter (\(end, dt) -> end /= n)) edges'
  modify $ \g -> g {nodes = nodes', edges = edges''}

deleteEdge :: (Eq ni, Ord ni) => ni -> ni -> GraphBuilder ni nd ed ()
deleteEdge start end = do
  g <- get
  let edges' = Map.update (Just <$> filter (\(e, dt) -> e /= end)) start (edges g)
  modify $ \g -> g {edges = edges'}

update :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Graph ni nd ed -> Either Text (Graph ni nd ed)
update bd init =
  let (except, g) = (runState $ runExceptT $ buildGraph bd) init
   in case except of
        Left except -> Left except
        Right _ -> Right g

build :: (Eq ni, Ord ni) => GraphBuilder ni nd ed a -> Either Text (Graph ni nd ed)
build bd = update bd empty

traverseM_ :: (Eq ni, Ord ni, Monad m) => (ni -> nd -> m a) -> Graph ni nd ed -> m ()
traverseM_ f g@Graph {nodes = nodes} = recurse initIndegree g
  where
    initIndegree = Map.mapWithKey (\i d -> length $ inBound i g) nodes
    findZeroIndegree :: Map ni Int -> [ni]
    findZeroIndegree m = fmap fst $ Map.toList $ Map.filter (== 0) m
    updateIndegree i m g =
      let m' = List.foldl' (\m (ni, _) -> Map.adjust (\x -> x - 1) ni m) m $ outBound i g
          m'' = Map.delete i m'
       in m''
    recurse indegree g = do
      let zeroIndegree = findZeroIndegree indegree
      case zeroIndegree of
        [] -> return ()
        (n : ns) -> do
          f n (Maybe.fromJust $ lookupNode n g)
          let indegree' = updateIndegree n indegree g
          recurse indegree' g

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
  evalState (unmem $ recurse_ (\i g -> outBound i g <&> fst) idx g) (Memory Set.empty Map.empty)

strictlyPostDominate :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> Set ni
strictlyPostDominate idx g =
  evalState (unmem $ recurse_ (\i g -> inBound i g <&> fst) idx g) (Memory Set.empty Map.empty)
