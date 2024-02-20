-- CFG -- Control Flow Graph with SSA nodes
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
import CFG.PartialCFG qualified as PCFG
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import SSA
import Semantic qualified as SE
import Types
import Util.Graph qualified as G

data VarBiMap = VarBiMap
  { varToSym :: Map VID (ScopeID, Name),
    symToVar :: Map (ScopeID, Name) VID
  }

addVarSym :: ScopeID -> Name -> VID -> VarBiMap -> VarBiMap
addVarSym sid name vid (VarBiMap varToSym symToVar) =
  let varToSym' = Map.insert vid (sid, name) varToSym
      symToVar' = Map.insert (sid, name) vid symToVar
   in VarBiMap varToSym' symToVar'

lookupVar :: VID -> VarBiMap -> Maybe (ScopeID, Name)
lookupVar vid VarBiMap {varToSym = m} = Map.lookup vid m

lookupSym :: ScopeID -> Name -> VarBiMap -> Maybe VID
lookupSym sid name VarBiMap {symToVar = m} = Map.lookup (sid, name) m

data Condition
  = Pred {pred :: VarOrImm}
  | Complement
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: ScopeID,
    statements :: [SSA]
  }
  deriving (Generic, Show)

data CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge

data CFGState = CFGState
  { cfg :: CFG,
    vars :: VarList,
    symMap :: VarBiMap,
    exitVar :: Map BBID Var
  }
  deriving (Generic)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SE.SymbolTable
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

-- buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
-- buildCFG (AST.ASTRoot _ _ methods) context = _

-- buildMethod :: PCFG.CFG -> CFGBuild CFG
-- buildMethod (G.Graph nodes edges) = G.traverseDF_ _
