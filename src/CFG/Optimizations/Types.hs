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

module CFG.Optimizations.Types where

import Data.Generics.Labels
import GHC.Generics (Generic)
import CFG.Types
import Control.Monad.Except
import Control.Monad.State
import Types
import Util.Graph qualified as G
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)

data CFGOptimizerState s = CFGOptimizerState
  { cfg :: CFG
  , optState :: s
  } deriving (Generic)

newtype CFGOptimizer s a = CFGOptmizer
  { runOptimizer :: StateT (CFGOptimizerState s) (Except CompileError) a
  } deriving (Functor, Applicative, Monad, MonadError CompileError, MonadState (CFGOptimizerState s))

runOptimizerOnCFG :: s -> CFGOptimizer s () -> CFG -> Either [CompileError] CFG
runOptimizerOnCFG initOptState opt cfg =
  let initState = CFGOptimizerState cfg initOptState
      result = runExcept $ runStateT (runOptimizer opt) initState
  in case result of
    Left err -> Left [err]
    Right (_, CFGOptimizerState cfg _) -> Right cfg

getCFG :: CFGOptimizer s CFG
getCFG = gets cfg

getOptState :: CFGOptimizer s s
getOptState = gets optState

updateCFG :: G.GraphBuilder BBID BasicBlock CFGEdge a -> CFGOptimizer s ()
updateCFG update = do
  (CFG g _ _) <- getCFG
  let g' = G.update update g
  case g' of
    Left m -> throwError $ CompileError Nothing m
    Right g -> #cfg . #graph .= g
