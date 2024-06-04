-- Types -- Common types
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
module Types where

import Data.Text (Text)

-- Variable ID
type VID = Int

-- Var/Function name
type Name = Text

-- Scope of function, basic block, etc.
type ScopeID = Int

-- Basic block ID
type BBID = Int