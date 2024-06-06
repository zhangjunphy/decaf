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

import GHC.Generics (Generic)
import SSA
import Types
import Util.Graph qualified as G

data Condition
  = Pred {pred :: !VarOrImm}
  | Complement
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: !BBID,
    sid :: !ScopeID,
    statements :: ![SSA]
  }
  deriving (Generic, Show)

newtype CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge !Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge
