-- Copyright (C) 2018-2024 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

module CFG.Types where

import Data.Generics.Labels
import GHC.Generics (Generic)
import SSA
import Types
import Util.Graph qualified as G
import Data.Map (Map)
import qualified AST

data Condition
  = Pred {pred :: !VarOrImm}
  | Complement {pred :: !VarOrImm}
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: !BBID,
    sid :: !ScopeID,
    statements :: ![SSA]
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge !Condition
  deriving (Show)

-- type CFG = G.Graph BBID CFGNode CFGEdge

data CFG = CFG
  { graph :: !(G.Graph BBID BasicBlock CFGEdge),
    entry :: !BBID,
    exit :: !BBID,
    arguments :: ![Var],
    sig :: !AST.MethodSig
  }
  deriving (Generic)

type CFGBuilder = G.GraphBuilder BBID BasicBlock CFGEdge

data SingleFileCFG = SingleFileCFG
  { declares :: ![Name],          -- Foreign functions
    global :: ![(Var, AST.Type)], -- Global variables
    cfgs :: !(Map Name CFG)
  } deriving (Generic)
