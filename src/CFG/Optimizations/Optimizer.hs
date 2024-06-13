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

module CFG.Optimizations.Optimizer where

import Data.Generics.Labels
import GHC.Generics (Generic)
import CFG.Types
import Control.Monad.Except
import Control.Monad.State
import Types
import Util.Graph qualified as G
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)

data CFGOptimizerState = CFGOptimizerState
  { cfg :: CFG
  } deriving (Generic)

newtype CFGOptimizer a = CFGOptmizer
  { runOptimizer :: StateT CFGOptimizerState (Except CompileError) a
  } deriving (Functor, Applicative, Monad, MonadError CompileError, MonadState CFGOptimizerState)

runOptimizerOnCFG :: CFGOptimizer () -> CFG -> Either [CompileError] CFG
runOptimizerOnCFG opt cfg =
  let initState = CFGOptimizerState cfg
      result = runExcept $ runStateT (runOptimizer opt) initState
  in case result of
    Left err -> Left [err]
    Right (_, CFGOptimizerState cfg) -> Right cfg

getCFG :: CFGOptimizer CFG
getCFG = gets cfg

updateCFG :: G.GraphBuilder BBID BasicBlock CFGEdge a -> CFGOptimizer ()
updateCFG update = do
  (CFG g _ _) <- getCFG
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CompileError Nothing m
    Right g -> #cfg . #graph .= g
