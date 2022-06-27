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

{-# LANGUAGE DuplicateRecordFields #-}
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
import Formatting

import Semantic
import CFG (CFG)
import qualified CFG
import qualified CFG as Node
import qualified IR


type Label = Text

data Condition
  = Always
  | Pred {pred :: IR.Expr}
  | Complement 
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SymbolTable
  }

data CFGNodeData = 
  StatementNode IR.Statement

data CFGEdgeData
  = SeqEdge
  | CondEdge Condition

data CFGState = CFGState
  { cfg :: CFG CFGNodeData CFGEdgeData,
    lastNode :: Maybe CFG.Node,
    sid :: IR.ScopeID
  }

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGMonad a = CFGMonad {runCFGMonad :: ExceptT CFGExcept (ReaderT CFGContext (State CFGState)) a}
  deriving (Functor, Applicative, Monad, MonadError CFGExcept, MonadReader CFGContext, MonadState CFGState)

buildCFGForMethod :: IR.MethodDecl -> CFGMonad ()
buildCFGForMethod method@(IR.MethodDecl sig (IR.Block vars stmts blockID)) =
  forM_ stmts processStatement

processStatement :: IR.Statement -> CFGMonad ()
processStatement st = case st of
  assign@(IR.AssignStmt _) -> addStatementNode assign
  methodCall@(IR.MethodCallStmt _) -> addStatementNode methodCall
  ifStmt@IR.IfStmt {} -> _
  _ -> _

addStatementNode :: IR.Statement -> CFGMonad ()
addStatementNode stmt = do
  cfg <- gets cfg
  lastNode <- gets lastNode
  node <- addNode $ StatementNode stmt
  case lastNode of
    Just lastN -> addEdge lastN node SeqEdge
    Nothing -> return ()

processIfBlock :: IR.Statement -> CFGMonad ()
processIfBlock = do
  cfg <- gets cfg
  lastNode <- gets lastNode
  node <- addNode $ _
  _

throwExcept :: Text -> CFGMonad a
throwExcept msg = throwError $ CFGExcept msg

addNode :: CFGNodeData -> CFGMonad CFG.Node
addNode nd = do
  cfg <- gets cfg
  let (node, cfg) = CFG.addNode nd cfg
  modify $ \s -> s {cfg = cfg}
  return node

addEdge :: CFG.Node -> CFG.Node -> CFGEdgeData -> CFGMonad ()
addEdge src target dt = do
  cfg <- gets cfg
  modify $ \s -> s {cfg = CFG.addEdge src target dt cfg}

addSelect :: IR.Expr -> CFG.Node -> CFG.Node -> CFG.Node -> CFGMonad ()
addSelect pred src true false = do
  addEdge src true $ CondEdge $ Pred pred
  addEdge src false $ CondEdge Complement

lookupSymT :: ScopeID -> CFGMonad (Maybe SymbolTable)
lookupSymT sid = do
  symTs <- reader symbolTables
  return $ Map.lookup sid symTs

lookupSymT' :: ScopeID -> CFGMonad SymbolTable
lookupSymT' sid = do
  symT <- lookupSymT sid
  case symT of
    (Just t) -> return t
    _ -> throwExcept $ sformat ("Symbol table not found for " % int) sid

lookupCurrentSymT' :: CFGMonad SymbolTable
lookupCurrentSymT' = do
  sid <- gets sid
  lookupSymT' sid

generateBlockLabel :: CFGMonad Label
generateBlockLabel = do
  symT <- lookupCurrentSymT'
  sid <- gets sid
  let tpe = blockType symT
  return $ sformat (stext % "_" % int) (blockType2Text tpe) sid
  where
    blockType2Text (MethodBlock (IR.MethodSig name _ _)) = sformat ("func_" % stext) name
    blockType2Text RootBlock = "root"
    blockType2Text IfBlock = "if"
    blockType2Text ForBlock = "for"
    blockType2Text WhileBlock = "while"
