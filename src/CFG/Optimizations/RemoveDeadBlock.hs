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

module CFG.Optimizations.RemoveDeadBlock where

import CFG.Optimizations.Optimizer
import CFG.Types
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
import Control.Monad.Except
import Data.List (find)
import Data.Map.Strict qualified as Map
import SSA (SSA)
import SSA qualified
import Types
import Util.Graph qualified as G

removeDeadBlock :: CFGOptimizer ()
removeDeadBlock = do
  cfg <- getCFG
  case findDeadNode cfg of
    Nothing -> return ()
    Just bbid -> do
      updateCFG $ G.deleteNode bbid
      removeDeadBlock

findDeadNode :: CFG -> Maybe BBID
findDeadNode (CFG g@(G.Graph nodes _) entry _) =
  fst <$> find (\(n, _) -> n /= entry && null (G.inBound n g)) (Map.toList nodes)
