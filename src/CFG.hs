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

-- CFG -- Control Flow Graph with SSA nodes
module CFG (plot, CFGContext (..), buildCFG, Condition (..), BasicBlock (..), CFGEdge (..), CFG (..)) where

import AST qualified
import CFG.Build (CFGContext (..), buildCFG)
import CFG.Optimizations.Optimizer (CFGOptimizer, runOptimizerOnCFG)
import CFG.Optimizations.RemoveDeadBlock (removeDeadBlock)
import CFG.Optimizations.RemoveNoOp (removeNoOp)
import CFG.Plot (generateDotPlot)
import CFG.Types
import Control.Monad (mapM_)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Semantic qualified as SE
import Types
import SSA

{-
Refactor and clean up.
TODO:
1. Find better ways to add phi nodes. [DROP]
2. Refactor control start/exit related code. [DONE]
3. Produce dot plot with some proper library. [DONE]
4. Add unit tests.
5. Other chores.
-}

optimizations :: [CFGOptimizer ()]
optimizations = [removeDeadBlock, removeNoOp]

generateCFG :: AST.ASTRoot -> SE.SemanticInfo -> Either [CompileError] (Map Name CFG)
generateCFG root si = do
  let context = CFGContext si
  cfgs <- buildCFG root context
  mapM (runOptimizerOnCFG (sequence_ optimizations)) cfgs

plot :: AST.ASTRoot -> SE.SemanticInfo -> Either [CompileError] String
plot root si = do
  cfgs <- generateCFG root si
  return $ Text.unpack $ mconcat $ Map.elems cfgs <&> generateDotPlot

-- linearize :: CFG -> [SSA]
-- linearize cfg = _
