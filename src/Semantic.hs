-- Semantic -- Semantic checker
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantic where

import           Control.Monad.State.Lazy
import           Data.ByteString.Lazy     (ByteString)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import qualified IR

-- semantic errors
newtype SemanticError = SemanticError ByteString
  deriving (Show)

-- symbol table definition
type ScopeID = Int
type SymbolSize = Int
data SymbolDef = SymbolDef { name :: IR.Name
                           , tpe  :: IR.Type
                           , size :: Int
                           } deriving (Show, Eq)
data SymbolTable = SymbolTable { scope   :: ScopeID
                               , parent  :: Maybe SymbolTable
                               , symbols :: Map IR.Name SymbolDef
                               } deriving Show
data SemanticState = SemanticState
  { nextScopeID  :: ScopeID
  , symbolTables :: Map ScopeID SymbolTable
  } deriving Show

-- traverse ir tree to compose symbol tables
newtype Semantic a = Semantic { runSemantic :: State SemanticState a }
  deriving (Functor, Applicative, Monad, MonadState SemanticState)

initialState :: Semantic ()
initialState = put $ SemanticState { nextScopeID = 0, symbolTables = Map.empty }

getNextScopeId :: Semantic Int
getNextScopeId = do
  state <- get
  return $ nextScopeID state

getSymbolTable :: ScopeID -> Semantic (Maybe SymbolTable)
getSymbolTable id = do
  state <- get
  return $ Map.lookup id $ symbolTables state

getLocalSymbol :: ScopeID -> IR.Name -> Semantic (Maybe SymbolDef)
getLocalSymbol id name = do
  table <- getSymbolTable id
  return $ table >>= (Map.lookup name <$> symbols)

getGlobalSymbol :: ScopeID -> IR.Name -> Semantic (Maybe SymbolDef)
getGlobalSymbol id name = do
  local <- getSymbolTable id
  return $ local >>= lookup name
  where lookup name table = case Map.lookup name (symbols table) of
          Nothing -> case (parent table) of
                       Nothing -> Nothing
                       Just p  -> lookup name p
          Just s -> Just s
