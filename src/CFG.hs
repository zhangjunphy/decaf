-- CFG -- Control Flow Graph
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

module CFG where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import Data.Text (Text)
import qualified Graph as G
import qualified IR
import qualified Semantic as SE

type Label = Text

data Condition
  = Pred {pred :: IR.Expr}
  | Complement
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map IR.ScopeID SE.SymbolTable
  }

data CFGNodeData
  = StatementNode IR.Statement

data CFGEdgeData
  = SeqEdge
  | CondEdge Condition

data CFGState = CFGState
  { cfg :: G.Graph CFGNodeData CFGEdgeData,
    sid :: IR.ScopeID
  }

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGMonad a = CFGMonad {runCFGMonad :: ExceptT CFGExcept (ReaderT CFGContext (State CFGState)) a}
  deriving (Functor, Applicative, Monad, MonadError CFGExcept, MonadReader CFGContext, MonadState CFGState)

