-- IR -- Decaf IR generator
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR ( generate
          ) where

import Parser
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map

----------------------------------------------------------------------
-- Walk through AST, generate an ir
----------------------------------------------------------------------

generate :: Parser.Program -> IRRoot
generate = \_ -> IRRoot

data IRRoot = IRRoot
              deriving (Show)

data AstNode = ProgramNode {}
