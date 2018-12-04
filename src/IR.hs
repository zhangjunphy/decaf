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

module IR ( generate
          ) where

import Text.Printf (printf)

import Parser
import Control.Monad.State

type Name = String

data FieldDescriptor
    = FieldDescriptor {
        fieldId   :: String
      , fieldIdx  :: Int
      , fieldTpe  :: Type
      , isVector  :: Bool
      , size      :: Int
      } deriving Show
data FieldSymTable = FieldSymTable {
      fieldTable :: [(String, FieldDescriptor)]
    , parentFieldTable :: FieldSymTable
    } deriving Show

data MethodDescriptor
    = MethodDescriptor {
        methodId   :: String
      , methodIdx  :: Int
      , returnType :: Maybe Type
      , statements :: [Statement]
      } deriving Show
data MethodSymTable = MethodSymTable {
      methodTable :: [(String, MethodDescriptor)]
    , parentMethodTable :: MethodSymTable
    } deriving Show

data CodegenState
    = CodegenState {
        currentBlock :: Name
      , fieldSymTable :: FieldSymTable
      , methodSymTable  :: MethodSymTable
      } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype IR a = IR (State Parser.Program a)
    deriving (Functor, Applicative, Monad, MonadState Parser.Program)

data AbstractSyntaxTree = AbstractSyntaxTree
                          deriving (Show)

data AstNode = ProgramNode {}

generate :: Parser.Program -> AbstractSyntaxTree
generate = \_ -> AbstractSyntaxTree
