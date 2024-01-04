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
  = Pred {pred :: AST.Expr}
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
    currentNode :: Maybe CFGNode,
    nextBbid :: BBID,
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
      currentNode = Nothing,
      nextBbid = 0,
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

buildStatement :: SL.Located AST.Statement -> CFGBuild ()
buildStatement (SL.LocatedAt _ stmt) = do
  g <- gets cfg
  node <- gets currentNode
  node' <- insertStmt node stmt
  modify (\s -> s {currentNode = Just node'})
  where
    insertStmt :: Maybe CFGNode -> AST.Statement -> CFGBuild CFGNode
    insertStmt (Just nd) stmt = do
      let block = bb nd
      let stmts = statements block
      return nd {bb = block {statements = stmts ++ [stmt]}}
    insertStmt Nothing stmt = do
      bbid <- gets nextBbid
      sid <- gets astScope
      return $ CFGNode bbid (BasicBlock bbid sid [stmt])
