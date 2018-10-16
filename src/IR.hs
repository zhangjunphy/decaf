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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR ( generate
          ) where

import Text.Printf (printf)

import qualified Parser
import Control.Monad.State

type Name = String

data Descriptor = FieldDescriptor
                | ParameterDescriptor

data SymbolTable = TypeSymbolTable
                 | FieldSymbolTable
                 | MethodSymbolTable

data CodegenState
    = CodegenState {
        currentBlock :: Name
      , blocks       :: [(Name, BlockState)]
      , symbolTable  :: SymbolTable
      }

data BlockState
    = BlockState {
      }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

data AbstractSyntaxTree = AbstractSyntaxTree
                        deriving (Show)

data AstNode = ProgramNode {}

generate :: Parser.Program -> AbstractSyntaxTree
generate = \_ -> AbstractSyntaxTree
