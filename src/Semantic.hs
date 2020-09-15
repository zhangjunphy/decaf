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

{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Semantic where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as B
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Text.Printf               (printf)

import qualified IR

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
newtype SemanticError = SemanticError ByteString
  deriving (Show)

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
newtype SemanticException = SemanticException ByteString
  deriving (Show)

-- symbol table definitions
type ScopeID = Int
type SymbolSize = Int
data SymbolDef = SymbolDef { name :: IR.Name
                           , tpe  :: IR.Type
                           , size :: Int
                           } deriving (Show, Eq)
data FunctionDef = FunctionDef { name :: IR.Name
                               , tpe  :: IR.Type
                               , size :: Int
                               } deriving (Show)
data SymbolTable = SymbolTable { scope     :: ScopeID
                               , parent    :: Maybe SymbolTable
                               , variables :: Map IR.Name SymbolDef
                               , functions :: Map IR.Name FunctionDef
                               } deriving Show
data SemanticState = SemanticState
  { nextScopeID  :: ScopeID
  , currentScope :: ScopeID
  , symbolTables :: Map ScopeID SymbolTable
  , blockRef     :: Map IR.Block ScopeID
  } deriving Show

-- Monad used for semantic analysis
-- Symbol tables are built for every scope, and stored in SemanticState.
-- Semantic errors encountered are recorded by the writer monad (WriterT [SemanticError]).
-- If a serious problem happened such that the analysis has to be aborted, a SemanticException
-- is thrown.
newtype Semantic a = Semantic { runSemantic :: ExceptT SemanticException (WriterT [SemanticError] (State SemanticState))  a }
  deriving (Functor, Applicative, Monad, MonadError SemanticException, MonadWriter [SemanticError], MonadState SemanticState)

throwSemanticException :: String -> Semantic a
throwSemanticException msg = throwError $ SemanticException $ B.fromString msg

-- find symbol table for current scope
getSymbolTable :: Semantic (Maybe SymbolTable)
getSymbolTable = do
  state <- get
  let id = currentScope state
  return $ Map.lookup id $ symbolTables state

-- find symbol table for current scope
-- will throw SemanticException if nothing is found
getSymbolTable' :: Semantic SymbolTable
getSymbolTable' = do
  t <- getSymbolTable
  case t of
    Nothing    -> throwSemanticException "No symble table found for current scope"
    Just table -> return table

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state { symbolTables = Map.insert (currentScope state) t (symbolTables state) }

-- lookup symbol in local scope.
-- this is useful to find naming conflicts.
lookupLocalSymbol :: IR.Name -> Semantic (Maybe SymbolDef)
lookupLocalSymbol name = do
  table <- getSymbolTable
  return $ table >>= (Map.lookup name . symbols)

lookupSymbol :: IR.Name -> Semantic (Maybe SymbolDef)
lookupSymbol name = do
  local <- getSymbolTable
  return $ local >>= lookup name
  where lookup name table = case Map.lookup name (symbols table) of
          -- found in local symbol table
          Just s -> Just s
          -- otherwise search in parent table
          Nothing -> case (parent table) of
                       Nothing -> Nothing
                       Just p  -> lookup name p

enterScope :: Semantic ()
enterScope = do
  state <- get
  parentST <- getSymbolTable
  let currentID = currentScope state
      nextID = nextScopeID state
      localST = SymbolTable { scope = nextID
                            , parent = parentST
                            , symbols = Map.empty
                            }
  put $ state { nextScopeID = nextID + 1
              , currentScope = nextID
              , symbolTables = Map.insert nextID localST $ symbolTables state
              }

exitScope :: Semantic ()
exitScope = do
  state <- get
  localST <- getSymbolTable
  case localST of
    Nothing ->
      throwSemanticException
        $ printf "No symbol table is associated with scope(%d)!" $ currentScope state
    Just table ->
      case parent table of
        Nothing ->
          throwSemanticException "Cannot exit root scope!"
        Just p ->
          put $ state { currentScope = scope p }

addSymbolDef :: SymbolDef -> Semantic ()
addSymbolDef def = do
  state <- get
  localST <- getSymbolTable'
  let symbols' = Map.insert (name def) def (symbols localST)
      newST = localST { symbols = symbols' }
  updateSymbolTable newST

-- traverse ir tree to compose symbol tables
semanticAnalysis :: IR.IRRoot -> Semantic ()
semanticAnalysis (IR.IRRoot imports vars methods) = do
  -- initial state
  put SemanticState { nextScopeID = 1
                    , currentScope = 0
                    , symbolTables = Map.empty
                    , blockRef = Map.empty
                    }
  _
