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
data SymbolTable = SymbolTable { scopeID   :: ScopeID
                               , parent    :: Maybe SymbolTable
                               , imports   :: Maybe (Map IR.Name IR.ImportDecl)
                               , variables :: Map IR.Name IR.FieldDecl
                               , methods   :: Maybe (Map IR.Name IR.MethodDecl)
                               } deriving Show
data SemanticState = SemanticState
  { nextScopeID    :: ScopeID
  , currentScopeID :: ScopeID
  , symbolTables   :: Map ScopeID SymbolTable
  , blockRef       :: Map IR.Block ScopeID
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

addSemanticError :: String -> Semantic ()
addSemanticError msg = tell $ [ SemanticError (B.fromString msg) ]

-- find symbol table for current scope
getSymbolTable :: Semantic (Maybe SymbolTable)
getSymbolTable = do
  state <- get
  let id = currentScopeID state
  return $ Map.lookup id $ symbolTables state

getCurrentScopeID :: Semantic Int
getCurrentScopeID = do
  state <- get
  return $ currentScopeID state

-- find symbol table for current scope
-- will throw SemanticException if nothing is found
getSymbolTable' :: Semantic SymbolTable
getSymbolTable' = do
  scopeID <- getCurrentScopeID
  t <- getSymbolTable
  case t of
    Nothing    -> throwSemanticException $
      printf "No symble table found for current scope %d" $ scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map IR.Name IR.FieldDecl)
getLocalVariables' = do
  localST <- getSymbolTable'
  return $ variables localST

getLocalImports' :: Semantic (Map IR.Name IR.ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case imports localST of
    Nothing -> throwSemanticException $ printf "No import table for scope %d" $ scopeID localST
    Just t -> return t

getLocalMethods' :: Semantic (Map IR.Name IR.MethodDecl)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methods localST of
    Nothing -> throwSemanticException $ printf "No method table for scope %d" $ scopeID localST
    Just t  -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state { symbolTables = Map.insert (currentScopeID state) t (symbolTables state) }

-- lookup symbol in local scope.
-- this is useful to find naming conflicts.
lookupLocalVariable :: IR.Name -> Semantic (Maybe IR.FieldDecl)
lookupLocalVariable name = do
  table <- getSymbolTable
  return $ table >>= (Map.lookup name . variables)

lookupVariable :: IR.Name -> Semantic (Maybe IR.FieldDecl)
lookupVariable name = do
  local <- getSymbolTable
  return $ local >>= lookup name
  where lookup name table = case Map.lookup name (variables table) of
          -- found in local symbol table
          Just s -> Just s
          -- otherwise search in parent table
          Nothing -> case (parent table) of
                       Nothing -> Nothing
                       Just p  -> lookup name p

enterScope :: IR.Block -> Semantic ()
enterScope block = do
  state <- get
  parentST <- getSymbolTable
  let currentID = currentScopeID state
      nextID = nextScopeID state
      localST = SymbolTable { scopeID = nextID
                            , parent = parentST
                            , variables = Map.empty
                            , imports = Nothing
                            , methods = Nothing
                            }
  put $ state { nextScopeID = nextID + 1
              , currentScopeID = nextID
              , symbolTables = Map.insert nextID localST $ symbolTables state
              , blockRef = Map.insert block nextID $ blockRef state
              }

exitScope :: Semantic ()
exitScope = do
  state <- get
  localST <- getSymbolTable
  case localST of
    Nothing ->
      throwSemanticException
        $ printf "No symbol table is associated with scope(%d)!" $ currentScopeID state
    Just table ->
      case parent table of
        Nothing ->
          throwSemanticException "Cannot exit root scope!"
        Just p ->
          put $ state { currentScopeID = scopeID p }

addVariableDef :: IR.FieldDecl -> Semantic ()
addVariableDef def = do
  localST <- getSymbolTable'
  -- check for duplications
  let nm = IR.name (def :: IR.FieldDecl)
  case Map.lookup (IR.name (def :: IR.FieldDecl)) (variables localST) of
    Just _ -> addSemanticError $ printf "duplicate definition for variable %d" $ B.toString nm
  let variables' = Map.insert (IR.name (def :: IR.FieldDecl)) def (variables localST)
      newST = localST { variables = variables' }
  -- ensure declared array size > 0
  case IR.size def of
    Just s -> if s > 0 then addSemanticError "" else tell []
  updateSymbolTable newST

addImportDef :: IR.ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  let nm = IR.name (def :: IR.ImportDecl)
  case Map.lookup (IR.name (def :: IR.ImportDecl)) importTable of
    Just _ -> addSemanticError $ printf "duplicate import %d" $ B.toString nm
  let imports' = Map.insert (IR.name (def :: IR.ImportDecl)) def importTable
      newST = localST { imports = Just imports' }
  updateSymbolTable newST

addMethodDef :: IR.MethodDecl -> Semantic ()
addMethodDef def = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  let nm = IR.name (def :: IR.MethodDecl)
  case Map.lookup (IR.name (def :: IR.MethodDecl)) methodTable of
    Just _ -> addSemanticError $ printf "duplicate definition for method %d" $ B.toString nm
  let methods' = Map.insert (IR.name (def :: IR.MethodDecl)) def methodTable
      newST = localST { methods = Just methods' }
  updateSymbolTable newST

-- traverse ir tree to compose symbol tables and check semantic errors
semanticAnalysis :: IR.IRRoot -> Semantic ()
semanticAnalysis (IR.IRRoot imports vars methods) = do
  -- initial state
  let globalST = SymbolTable { scopeID = 0
                             , parent = Nothing
                             , imports = Just Map.empty
                             , variables = Map.empty
                             , methods = Just Map.empty
                             }
  put SemanticState { nextScopeID = 1
                    , currentScopeID = 0
                    , symbolTables = Map.fromList [(0, globalST)]
                    , blockRef = Map.empty
                    }
  -- add imports to symbol table
  _
