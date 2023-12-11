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
import Data.Text (Text)

import Control.Monad.Except
import Control.Monad.State

newtype Node a = Node a
  deriving (Eq, Ord, Show)

data Graph ni nd ed = Graph
  { nodes :: Map (Node ni) nd,
    edges :: Map (Node ni) [(Node ni, ed)],
    start :: Maybe (Node ni)
  }
  deriving (Show)

type GraphException = Text

empty :: Graph ni nd ed
empty = Graph Map.empty Map.empty Nothing

outBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(Node ni, ed)]
outBound idx g = concat $ Map.lookup (Node idx) (edges g)

inBound :: (Eq ni, Ord ni) => ni -> Graph ni nd ed -> [(Node ni, ed)]
inBound idx cfg =
  let edges' = Map.mapMaybe (Just <$> filter (\(end, _) -> end == Node idx)) (edges cfg)
      nodes' =
        concat (Map.assocs edges' <&> \(src, dsts) -> zip (repeat src) dsts)
          <&> \(src, (_, ed)) -> (src, ed)
   in nodes'

newtype GraphBuilder ni nd ed a = GraphBuilder
  {buildGraph :: (ExceptT GraphException (State (Graph ni nd ed))) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError GraphException,
      MonadState (Graph ni nd ed)
    )

addNode :: (Eq ni, Ord ni) => ni -> nd -> GraphBuilder ni nd ed (Node ni)
addNode idx dt = do
  let n = Node idx
  modify $ \g -> g{nodes = Map.insert n dt (nodes g)}
  return n

addEdge :: (Eq ni, Ord ni) => Node ni -> Node ni -> ed -> GraphBuilder ni nd ed ()
addEdge start end dt = do
  g <- get
  let edges' = Map.insertWith (++) start [(end, dt)] (edges g)
  modify $ \g -> g{edges = edges'}

deleteNode :: (Eq ni, Ord ni) => Node ni -> GraphBuilder ni nd ed ()
deleteNode node = do 
  g <- get
  let nodes' = Map.delete node (nodes g)
      edges' = Map.filterWithKey (\k v -> k /= node) (edges g)
      edges'' = Map.mapMaybe (Just <$> filter (\(end, dt) -> end /= node)) edges'
  modify $ \g -> g{nodes = nodes'}

deleteEdge :: (Eq ni, Ord ni) => Node ni -> Node ni -> GraphBuilder ni nd ed ()
deleteEdge start end = do
  g <- get
  let edges' = Map.update (Just <$> filter (\(e, dt) -> e /= end)) start (edges g)
  modify $ \g -> g{edges = edges'}

update :: (Eq ni, Ord ni) => GraphBuilder ni nd ed () -> Graph ni nd ed -> Either String (Graph ni nd ed)
update bd init =
  let (except, g) = (runState $ runExceptT $ buildGraph bd) init
   in case except of
    Left except -> Left $ show except
    Right _ -> Right g

build :: (Eq ni, Ord ni) => GraphBuilder ni nd ed () -> Either String (Graph ni nd ed)
build bd = update bd Graph.empty
