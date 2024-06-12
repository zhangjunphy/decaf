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
module CFG (plot, CFGContext (..), buildCFG, Condition (..), BasicBlock (..), CFGNode (..), CFGEdge (..), CFG (..)) where

import CFG.Build (CFGContext (..), buildCFG)
import CFG.Plot (plot)
import CFG.Types

{-
Refactor and clean up.
TODO:
1. Find better ways to add phi nodes. [DROP]
2. Refactor control start/exit related code. [DONE]
3. Produce dot plot with some proper library.
4. Add unit tests.
5. Other chores.
-}

-- Generate a dot plot for cfg

-- Remove empty seq node one by one
-- removeEmptySeqNode :: CFGBuild ()
-- removeEmptySeqNode = do
--  g@G.Graph {nodes = nodes} <- gets cfg
--  let emptySeqNode =
--        List.find
--          (\(ni, nd) -> isEmptyNode nd && isSeqOut ni g)
--          $ Map.assocs nodes
--  case emptySeqNode of
--    Nothing -> return ()
--    Just (ni, _) -> do
--      updateCFG $
--        let inEdges = G.inBound ni g
--            outEdge = head $ G.outBound ni g
--         in bridgeEdges ni inEdges outEdge
--      removeEmptySeqNode
--  where
--    isEmptyNode CFGNode {bb = BasicBlock {statements = stmts}} = null stmts
--    isSeqOut ni g =
--      let outEdges = G.outBound ni g
--       in length outEdges == 1 && isSeqEdge (snd $ head outEdges)
--    bridgeEdges mid inEdges (out, _) = do
--      G.deleteNode mid
--      forM_ inEdges (\(ni, ed) -> G.addEdge ni out ed)
--    isSeqEdge SeqEdge = True
--    isSeqEdge _ = False
