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

-- Parser -- Re-export Happy parser functionalities
module Parser
  ( parse,
    Program (..),
    ImportDecl (..),
    FieldDecl (..),
    MethodDecl (..),
    FieldElem (..),
    Type (..),
    Argument (..),
    Block (..),
    Statement (..),
    Location (..),
    AssignExpr (..),
    MethodCall (..),
    ImportArg (..),
    CounterUpdate (..),
    Expr (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import Lexer (Alex (..), Token, runAlex)
import Parser.Grammar
import Parser.Tree
import Util.SourceLoc as SL

parse :: ByteString -> Either String Program
parse input = runAlex input parseInternal
