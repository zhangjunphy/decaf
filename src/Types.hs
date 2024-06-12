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

-- Types -- Common types
module Types (VID, Name, ScopeID, BBID, CompileError(..)) where

import Data.Text (Text)
import Util.SourceLoc qualified as SL
import Formatting (sformat, int, stext, (%), formatToString)

-- Variable ID
type VID = Int

-- Var/Function name
type Name = Text

-- Scope of function, basic block, etc.
type ScopeID = Int

-- Basic block ID
type BBID = Int

data CompileError = CompileError
  { sl :: !(Maybe SL.Range)
  , msg :: !Text
  }

instance Show CompileError where
  show (CompileError (Just sl) msg) = formatToString (int % ":" % int % ": error: " % stext) (SL.row (SL.start sl) + 1) (SL.col (SL.start sl) + 1) msg
  show (CompileError Nothing msg) = formatToString ("error: " % stext) msg
