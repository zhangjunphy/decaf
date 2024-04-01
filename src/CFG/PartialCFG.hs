-- PartialCFG -- Control Flow Graph with unconverted AST information
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
module CFG.PartialCFG where

import AST qualified
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Formatting
import GHC.Generics (Generic)
import Semantic qualified as SE
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL
import Types

data Condition
  = Pred {pred :: AST.Expr}
  | Complement
  deriving (Show)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SE.SymbolTable
  }

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: ScopeID,
    statements :: [AST.Statement]
  }
  deriving (Generic, Show)

data CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge { sid :: ScopeID }
  | CondEdge { sid :: ScopeID, cond :: Condition}
  deriving (Generic, Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge

data CFGState = CFGState
  { cfg :: CFG,
    astScope :: ScopeID,
    nextBBID :: BBID,
    statements :: [AST.Statement]
  }
  deriving (Generic, Show)

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

initialCFGState :: ScopeID -> CFGState
initialCFGState sid =
  CFGState
    { cfg = G.empty,
      astScope = sid,
      nextBBID = 0,
      statements = []
    }

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
buildCFG (AST.ASTRoot _ _ methods) context =
  let build method =
        runState $
          flip runReaderT context $
            runExceptT $
              runCFGBuild (buildMethod method)
      updateMap map m =
        let (r, _) = build m $ initialCFGState $ m ^. (#block . #blockID)
         in case r of
              Left e -> Left e
              Right r' -> Right $ Map.insert (m ^. (#sig . #name)) r' map
   in foldM updateMap Map.empty methods

-- Helps for CFGBuild monad
consumeBBID :: CFGBuild BBID
consumeBBID = do
  bbid <- use #nextBBID
  #nextBBID += 1
  return bbid

updateCFG :: G.GraphBuilder BBID CFGNode CFGEdge a -> CFGBuild ()
updateCFG update = do
  g <- use #cfg
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CFGExcept m
    Right g -> #cfg .= g

appendStatement :: AST.Statement -> CFGBuild ()
appendStatement stmt = #statements %= (++ [stmt])

createIsolateBB :: CFGBuild BBID
createIsolateBB = do
  bbid <- consumeBBID
  stmts <- use #statements
  sid <- use #astScope
  #statements .= []
  let bb = BasicBlock bbid sid stmts
  updateCFG (G.addNode bbid (CFGNode bb))
  return bbid

checkStmts :: CFGBuild ()
checkStmts = do
  stmts <- use #statements
  unless (null stmts) $ throwError $ CFGExcept $ Text.pack $ "Dangling statements found: " ++ show stmts

-- CFG Builders
buildMethod :: AST.MethodDecl -> CFGBuild CFG
buildMethod method@AST.MethodDecl {sig = sig, block = block@(AST.Block _ stmts sid)} = do
  checkStmts
  buildBlock block
  removeEmptySeqNode
  gets cfg

buildBlock :: AST.Block -> CFGBuild (BBID, BBID)
buildBlock block@AST.Block {stmts = stmts} = do
  checkStmts
  -- always create an empty BB at the start
  head <- createIsolateBB
  stmtTail <- foldM buildStatement head stmts
  tail <-
    if stmtTail == head
      then do
        id <- createIsolateBB
        sid <- use #astScope
        updateCFG (G.addEdge head id $ SeqEdge sid)
        return id
      else return stmtTail
  return (head, tail)

buildStatement :: BBID -> AST.Statement -> CFGBuild BBID
buildStatement prev stmt@(AST.Statement (AST.IfStmt pred ifBlock elseBlock) _) = do
  appendStatement stmt
  head <- createIsolateBB
  (ifStart, ifEnd) <- buildBlock ifBlock
  (elseStart, elseEnd) <- buildBlock ifBlock
  tail <- createIsolateBB
  sid <- use #astScope
  let gUpdate = do
        G.addEdge prev head $ SeqEdge sid
        G.addEdge head ifStart $ CondEdge sid $ Pred pred
        G.addEdge head elseStart $ CondEdge sid Complement
        G.addEdge ifEnd tail $ SeqEdge sid
        G.addEdge elseEnd tail $ SeqEdge sid
  updateCFG gUpdate
  return tail
buildStatement prev stmt@(AST.Statement AST.ForStmt{pred = pred, block = body} _) = do
  head <- createIsolateBB
  appendStatement stmt
  predBB <- createIsolateBB
  (bodyHead, bodyTail) <- buildBlock body
  tail <- createIsolateBB
  sid <- use #astScope
  let gUpdate = do
        G.addEdge prev head $ SeqEdge sid
        G.addEdge head predBB $ SeqEdge sid
        G.addEdge predBB bodyHead $ CondEdge sid $ Pred pred
        G.addEdge predBB tail $ CondEdge sid Complement
        G.addEdge bodyTail predBB $ SeqEdge sid
  updateCFG gUpdate
  return tail
buildStatement prev stmt@(AST.Statement AST.WhileStmt {pred = pred, block = body} _) = do
  head <- createIsolateBB
  appendStatement stmt
  predBB <- createIsolateBB
  (bodyHead, bodyTail) <- buildBlock body
  tail <- createIsolateBB
  sid <- use #astScope
  let gUpdate = do
        G.addEdge prev head $ SeqEdge sid
        G.addEdge head predBB $ SeqEdge sid
        G.addEdge predBB bodyHead $ CondEdge sid $ Pred pred
        G.addEdge predBB tail $ CondEdge sid Complement
        G.addEdge bodyTail predBB $ SeqEdge sid
  updateCFG gUpdate
  return tail
buildStatement head stmt = do
  appendStatement stmt
  return head

