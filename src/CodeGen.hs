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
import Formatting
import qualified IR
import Semantic

type Label = Text

data Condition
  = Always
  | Pred {pred :: IR.Expr}
  | Comp {pred :: IR.Expr}
  deriving (Show)

data CFGNode
  = CFGNode
      { label :: Label,
        scopeID :: ScopeID,
        stmts :: [IR.Statement]
      }
  | DummyNode Int
  deriving (Show)

data CFGEdge = CFGEdge
  { source :: CFGNode,
    target :: CFGNode,
    cond :: Condition
  }

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SymbolTable
  }

data CFG = CFG
  { getNodes :: [CFGNode],
    getEdges :: [CFGEdge]
  }

data CFGState = CFGState
  { sid :: ScopeID,
    statements :: [IR.Statement],
    getCFG :: CFG,
    nodeID :: Int,
    dummyNodeID :: Int
  }

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGMonad a = CFGMonad {runCFGMonad :: ExceptT CFGExcept (ReaderT CFGContext (State CFGState)) a}
  deriving (Functor, Applicative, Monad, MonadError CFGExcept, MonadReader CFGContext, MonadState CFGState)

throwExcept :: Text -> CFGMonad a
throwExcept msg = throwError $ CFGExcept msg

putStatement :: IR.Statement -> CFGMonad ()
putStatement stm = do
  s@CFGState {statements = stms} <- get
  put s {statements = stms ++ [stm]}

getStatements :: CFGMonad [IR.Statement]
getStatements = gets statements

incNodeID :: CFGMonad ()
incNodeID = do
  id <- gets nodeID
  modify $ \s -> s {nodeID = id + 1}

addNode :: CFGNode -> CFGMonad ()
addNode node = do
  cfg <- gets getCFG
  let nodes = getNodes cfg
  modify $ \s -> s {getCFG = cfg {getNodes = nodes ++ [node]}}

addDummyNode :: CFGMonad CFGNode
addDummyNode = do
  dummyID <- gets dummyNodeID
  modify $ \s -> s {dummyNodeID = dummyID + 1}
  return (DummyNode dummyID)

addEdge :: Condition -> CFGNode -> CFGNode -> CFGMonad ()
addEdge cond src target = do
  cfg <- gets getCFG
  let edges = getEdges cfg
      new = CFGEdge src target cond
  modify $ \s -> s {getCFG = cfg {getEdges = edges ++ [new]}}

addSelect :: IR.Expr -> CFGNode -> CFGNode -> CFGNode -> CFGMonad ()
addSelect pred src true false = do
  addEdge (Pred pred) src true
  addEdge (Comp pred) src false

addAlways :: CFGNode -> CFGNode -> CFGMonad ()
addAlways = addEdge Always

lookupSymT :: ScopeID -> CFGMonad (Maybe SymbolTable)
lookupSymT sid = do
  symTs <- reader symbolTables
  return $ Map.lookup sid symTs

lookupSymT' :: ScopeID -> CFGMonad SymbolTable
lookupSymT' sid = do
  symT <- lookupSymT sid
  case symT of
    (Just t) -> return t
    _ -> throwExcept $ sformat ("Symbole table not found for " % int) sid

lookupCurrentSymT' :: CFGMonad SymbolTable
lookupCurrentSymT' = do
  sid <- gets sid
  lookupSymT' sid

generateBlockLabel :: CFGMonad Label
generateBlockLabel = do
  symT <- lookupCurrentSymT'
  nodeID <- gets nodeID
  sid <- gets sid
  let tpe = blockType symT
  return $ sformat (stext % "_" % int % "_" % int) (blockType2Text tpe) sid nodeID
  where
    blockType2Text (MethodBlock (IR.MethodSig name _ _)) = sformat ("func_" % stext) name
    blockType2Text RootBlock = "root"
    blockType2Text IfBlock = "if"
    blockType2Text ForBlock = "for"
    blockType2Text WhileBlock = "while"

buildNode :: CFGMonad CFGNode
buildNode = do
  stms <- getStatements
  sid <- gets sid
  label <- generateBlockLabel
  incNodeID
  return $ CFGNode label sid stms

constructCFG :: IR.IRRoot -> CFGMonad [CFGNode]
constructCFG (IR.IRRoot imports decls methods) = do
  let topLevel = CFGNode topLevelLabel globalScopeID allocations
  return [topLevel]
  where
    allocations = decls <&> IR.VarDeclStmt

constructMethod :: IR.MethodDecl -> CFGMonad CFGNode
constructMethod (IR.MethodDecl sig block@(IR.Block fieldDecls statements sid)) = do
  forM_ fieldDecls (putStatement . IR.VarDeclStmt)
  forM_ statements constructStatement
  _

constructBlock :: IR.Block -> CFGMonad (CFGNode, CFGNode)
constructBlock (IR.Block fieldDecls statements sid) = do
  forM_ fieldDecls (putStatement . IR.VarDeclStmt)
  forM_ statements constructStatement
  inNode <- addDummyNode
  outNode <- addDummyNode
  _

constructStatement :: IR.Statement -> CFGMonad (Maybe (CFGNode, CFGNode))
constructStatement
  stm@( IR.ForStmt
          nm
          (IR.WithType init _)
          (IR.WithType pred _)
          (IR.Assignment (IR.WithType loc _) op expr)
          block
        ) = do
  _
constructStatement stm@(IR.WhileStmt (IR.WithType pred _) block) = do
  (bodyIn, bodyOut) <- constructBlock block
  inNode <- addDummyNode
  outNode <- addDummyNode
  addSelect pred inNode bodyIn outNode
  addAlways bodyOut inNode
  return $ Just (inNode, outNode)
constructStatement stm@(IR.IfStmt (IR.WithType pred _) ifBlock elseBlock) = do
  (ifIn, ifOut) <- constructBlock ifBlock
  elseNode <- mapM constructBlock elseBlock
  inNode <- addDummyNode
  outNode <- addDummyNode
  case elseNode of
    Nothing -> do
      addSelect pred inNode ifIn outNode
      addAlways ifOut outNode
    (Just (elseIn, elseOut)) -> do
      addSelect pred inNode ifIn elseIn
      addAlways ifOut outNode
      addAlways elseOut outNode
  return $ Just (inNode, outNode)
constructStatement stm = do
  putStatement stm
  return Nothing
