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
3. Produce dot plot with some proper library. [DONE]
4. Add unit tests. 
5. Other chores.
-}
