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

module CFG.Optimizations.RemoveNoOp where

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

removeNoOp :: CFGOptimizer ()
removeNoOp = do
  cfg <- getCFG
  case findNoOpNode cfg of
    Nothing -> return ()
    Just bbid -> do
      removeNodeAndPatchPhi bbid
      removeNoOp

-- find the first no-op node
findNoOpNode :: CFG -> Maybe BBID
findNoOpNode (CFG g@(G.Graph nodes edges) entry exit _ _) =
  fst <$> find (uncurry pred) (Map.toList nodes)
  where
    noOpPred bbid node
      | bbid == entry = False
      | bbid == exit = False
      | otherwise = null $ node ^. #statements
    inboundPred bbid _ = length (G.inBound bbid g) == 1
    outboundPred bbid _ = length (G.outBound bbid g) == 1
    outEdgePred bbid _ =
      let (_, bbidOut, edgeOut) = head (G.outBound bbid g)
       in case edgeOut of
            SeqEdge -> True
            _ -> False
    pred bbid node =
      noOpPred bbid node
        && inboundPred bbid node
        && outboundPred bbid node
        && outEdgePred bbid node

removeNodeAndPatchPhi :: BBID -> CFGOptimizer ()
removeNodeAndPatchPhi bbid = do
  -- no-op should have only 1 inbound and 1 outbound
  (CFG g _ _ _ _) <- getCFG
  let inbound = G.inBound bbid g
  let outbound = G.outBound bbid g
  let (bbidIn, _, edgeIn) = head inbound
  let (_, bbidOut, edgeOut) = head outbound
  -- udpate destination inbound edge
  updateCFG $ do
    G.deleteEdge bbidIn bbid
    G.deleteEdge bbid bbidOut
    G.addEdge bbidIn bbidOut edgeIn
  -- patch Phi in successor nodes
  updateCFG $ do
    G.adjustNode bbidOut (patchBasicBlock bbidIn)
    G.deleteNode bbid
  where
    isSeqEdge SeqEdge = True
    isSeqEdge _ = False
    patchPhi :: BBID -> SSA.SSA -> SSA.SSA
    patchPhi bbidIn (SSA.Phi dst predecessors) =
      let replace (var, bbid') = if bbid' == bbid then (var, bbidIn) else (var, bbid')
       in SSA.Phi dst $ replace <$> predecessors
    patchPhi _ ssa = ssa
    patchBasicBlock :: BBID -> BasicBlock -> BasicBlock
    patchBasicBlock bbidIn node = node & #statements %~ fmap (patchPhi bbidIn)
