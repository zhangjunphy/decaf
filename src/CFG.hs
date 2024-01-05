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
module CFG where

import AST qualified
import Control.Lens (view, (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Graph qualified as G
import Semantic qualified as SE
import SourceLoc qualified as SL

type Label = Text

-- Basic block ID
type BBID = Int

-- Variable ID
type VID = Int

data Condition
  = Pred {pred :: AST.Typed AST.Expr}
  | Complement
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map AST.ScopeID SE.SymbolTable
  }

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: AST.ScopeID,
    statements :: [AST.Statement]
  }
  deriving (Generic, Show)

data CFGNode = CFGNode
  { bbid :: BBID,
    bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

data CFGState = CFGState
  { cfg :: CFG,
    currentNode :: CFGNode,
    previousNode :: Maybe CFGNode,
    astScope :: AST.ScopeID
  }

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGBuild a = CFGBuild
  { runCFGBuild ::
      ExceptT
        CFGExcept
        (ReaderT CFGContext (State CFGState))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError CFGExcept,
      MonadReader CFGContext,
      MonadState CFGState
    )

initialCFGState :: AST.ScopeID -> CFGState
initialCFGState sid =
  CFGState
    { cfg = G.empty,
      currentNode = CFGNode 0 (BasicBlock 0 sid []),
      previousNode = Nothing,
      astScope = sid
    }

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map AST.Name CFG)
buildCFG (AST.ASTRoot _ _ methods) context =
  let build method =
        runState $
          flip runReaderT context $
            runExceptT $
              runCFGBuild (buildMethod method)
      updateMap map (SL.LocatedAt _ m) =
        let (r, _) = build m $ initialCFGState $ m ^. (#block . #blockID)
         in case r of
              Left e -> Left e
              Right r' -> Right $ Map.insert (m ^. (#sig . #name)) r' map
   in foldM updateMap Map.empty methods

buildMethod :: AST.MethodDecl -> CFGBuild CFG
buildMethod method@AST.MethodDecl {sig = sig, block = (AST.Block _ stmts sid)} = do
  mapM_ buildStatement stmts
  gets cfg

buildBlock :: AST.Block -> CFGBuild (CFGNode, CFGNode)
buildBlock block = do
  let stmts = block ^. #stmts
      sid = block ^. #blockID
  previousBB <- gets previousNode
  currentBB <- gets currentNode
  let bbid = currentBB ^. #bbid
  let nextBB = CFGNode (bbid + 1) (BasicBlock (bbid + 1) sid [])
  g <- gets cfg
  let gUpdate = do
        mapM_ (\node -> G.addEdge (node ^. #bbid) bbid SeqEdge) previousBB
      g' = G.update gUpdate g
  case g' of
    Left m -> throwError $ CFGExcept m
    Right g -> modify (\s -> s {currentNode = nextBB, cfg = g})
  last <- foldM (\_ s -> do
            n <- buildStatement s
            return $ Just n) Nothing stmts
  return (nextBB, fromMaybe nextBB last)

appendStatement :: AST.Statement -> CFGBuild ()
appendStatement stmt = do
  node <- gets currentNode
  let block = bb node
      stmts = statements block
      node' = node {bb = block {statements = stmts ++ [stmt]}}
  modify (\s -> s {currentNode = node'})

buildStatement :: SL.Located AST.Statement -> CFGBuild CFGNode
buildStatement (SL.LocatedAt _ stmt@(AST.IfStmt pred ifBlock elseBlock)) = do
  appendStatement stmt
  previousBB <- gets previousNode
  currentBB <- gets currentNode
  g <- gets cfg
  let bbid = currentBB ^. #bbid
  let sid = currentBB ^. (#bb . #sid)
  let nextBB = CFGNode (bbid + 1) (BasicBlock (bbid + 1) sid [])
  (ifNodeStart, ifNodeEnd) <- buildBlock ifBlock
  (elseNodeStart, elseNodeEnd) <- buildBlock ifBlock
  let gUpdate = do
        node <- G.addNode bbid currentBB
        let ifStartID = ifNodeStart ^. #bbid
            ifEndID = ifNodeEnd ^. #bbid
            elseStartID = elseNodeStart ^. #bbid
            elseEndID = elseNodeEnd ^. #bbid
        G.addEdge bbid ifStartID $ CondEdge $ Pred $ SL.unLocate pred
        G.addEdge bbid elseStartID $ CondEdge Complement
        G.addEdge ifEndID (bbid + 1) SeqEdge
        G.addEdge elseEndID (bbid + 1) SeqEdge
        mapM_ (\node -> G.addEdge (node ^. #bbid) bbid SeqEdge) previousBB
      g' = G.update gUpdate g
  case g' of
    Left m -> throwError $ CFGExcept m
    Right g -> modify (\s -> s {currentNode = nextBB, cfg = g})
  return nextBB
buildStatement (SL.LocatedAt _ stmt) = do
  appendStatement stmt
  gets currentNode
