{-# LANGUAGE DuplicateRecordFields #-}
-- Semantic -- Decaf IR generator and semantic checker
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
{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import Constants
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified IR
import Semantic

type Label = Text

data Condition = Condition
  deriving (Show)

data CFGNode = CFGNode
  { label :: Label,
    stmts :: [IR.Statement],
    prev :: Maybe CFGNode,
    next :: (Maybe CFGNode, Maybe (Condition, CFGNode))
  }
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SymbolTable
  }

data CFGState = CFGState
  { sid :: ScopeID
  }

newtype CFGMonad a = CFGMonad {runCFGMonad :: Reader CFGContext a}
  deriving (Functor, Applicative, Monad, MonadReader CFGContext)

constructCFG :: IR.IRRoot -> CFGMonad [CFGNode]
constructCFG (IR.IRRoot imports decls methods) = do
  let topLevel = CFGNode topLevelLabel allocations Nothing (Nothing, Nothing)
  return [topLevel]
  where
    allocations = decls <&> IR.VarDeclStmt

constructMethod :: IR.MethodDecl -> CFGMonad CFGNode
constructMethod (IR.MethodDecl sig (IR.Block fieldDecls statements sid)) = _
  where
    allocations = fieldDecls <&> IR.VarDeclStmt

constructBlock :: IR.Block -> CFGMonad CFGNode
constructBlock (IR.Block fieldDecls statements sid) = _
  where
    allocations = fieldDecls <&> IR.VarDeclStmt
