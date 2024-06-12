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
import Lexer (Alex (..), Token, runAlex, getAlexState, AlexUserState(..), AlexState(..))
import Parser.Grammar
import Parser.Tree
import Util.SourceLoc as SL
import Data.Text qualified as Text
import Types

parse :: ByteString -> Either [CompileError] Program
parse input = case runAlex input parseAndHandleError of
        Left m -> Left [CompileError Nothing $ Text.pack m]
        Right (errs, _) | not $ null errs -> Left errs
        Right (_, program) -> Right program
  where
    parseAndHandleError = parseInternal >>= \program -> do
      state <- getAlexState
      let AlexUserState{errors = errors} = alex_ust state 
      return (errors, program)
