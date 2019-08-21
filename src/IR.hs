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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module IR ( generate
          ) where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map                  as Map
import           Data.Maybe
import           Text.Read

import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as B

import qualified Parser                    as P

----------------------------------------------------------------------
-- IR tree
----------------------------------------------------------------------

type Name = ByteString

data BinaryOp =
  Plus
  | Minus
  deriving (Show)

data UnaryOp =
  Negate
  | Negative
  deriving (Show)

data TernaryOp =
  Choice
  deriving (Show)

data AssignOp =
  EqlAssign
  | IncAssign
  | DecAssign
  deriving (Show)

data IncrementOp =
  PlusPlus
  | MinusMinus
  deriving (Show)

data Type =
  IntType
  | BoolType
  deriving (Show)

data IRNode = IRRoot [IRNode] [IRNode] [IRNode]
            -- Declarations
            | ImportDecl Name
            | FieldDecl Type Name (Maybe Int)
            | MethodDecl (Maybe Type) Name [(Name, Type)] [IRNode]
            -- statement nodes
            | AssignStmt AssignOp IRNode IRNode
            | IncrementAssignStmt IncrementOp IRNode
            | IfStmt IRNode [IRNode] [IRNode]
            | ForStmt IRNode IRNode IRNode [IRNode]
            | WhileStmt IRNode [IRNode]
            | ReturnStmt IRNode
            | BreakStmt
            | ContinueStmt
            -- experssion nodes
            | LocationExpr Name (Maybe IRNode)
            | MethodCallExpr Name [IRNode]
            | ExternCallExpr Name [IRNode]
            | IntLiteralExpr Int
            | BoolLiteralExpr Bool
            | BinaryOpExpr BinaryOp IRNode IRNode
            | UnaryOpExpr UnaryOp IRNode
            | LengthExpr Name
            | TernaryOpExpr TernaryOp IRNode IRNode IRNode
            deriving (Show)

----------------------------------------------------------------------
-- Reformat the parser tree into an IR tree
----------------------------------------------------------------------

newtype SemanticError = SemanticError ByteString
  deriving (Show)

data IRGenError = IRGenError { pos :: (Int, Int), message :: ByteString }
                deriving (Show)

newtype IRGenState = IRGenState
  { errors :: [IRGenError]
  } deriving (Show)

newtype IRGen a = IRGen { runIRGen :: State IRGenState a }
  deriving (Functor, Applicative, Monad, MonadState IRGenState)

addError :: IRGenError -> IRGen ()
addError e = do
  errs <- gets errors
  modify $ \s -> s {errors = errs ++ [e]}

generate :: P.Program ->  IRNode
generate (P.Program imports fields methods) = IRRoot [] [] []

irgenType :: P.Type -> Type
irgenType P.IntType  = IntType
irgenType P.BoolType = BoolType

irgenImportDecl :: P.ImportDecl -> IRNode
irgenImportDecl (P.ImportDecl id) = ImportDecl id

irgenFieldDecl :: P.FieldDecl -> [IRNode]
irgenFieldDecl (P.FieldDecl tpe elems) =
  flip fmap elems $ \case
      (P.ScalarField id)
        -> FieldDecl (irgenType tpe) id Nothing
      (P.VectorField id size)
        -> FieldDecl (irgenType tpe) id (Just sz)
           where sz = read $ B.toString size
