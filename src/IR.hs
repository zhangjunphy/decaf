-- IR -- Decaf IR elements
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IR where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.UTF8 as B
import Text.Printf (printf)

type Name = ByteString

type ScopeID = Int

-- operators
data RelOp
  = LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  deriving (Show, Eq)

data ArithOp
  = Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  deriving (Show, Eq)

data EqOp
  = Equal
  | NotEqual
  deriving (Show, Eq)

data CondOp
  = OR
  | AND
  deriving (Show, Eq)

data NegOp
  = Neg
  deriving (Show, Eq)

data NotOp
  = Not
  deriving (Show, Eq)

data ChoiceOp
  = Choice
  deriving (Show, Eq)

data AssignOp
  = EqlAssign
  | IncAssign
  | DecAssign
  | PlusPlus
  | MinusMinus
  deriving (Show, Eq)

data Type
  = IntType
  | BoolType
  | StringType
  | ArrayType Type
  deriving (Show, Eq)

parseArithOp :: ByteString -> ArithOp
parseArithOp op = case op of
  "+" -> Plus
  "-" -> Minus
  "*" -> Multiply
  "/" -> Division
  "%" -> Modulo

parseRelOp :: ByteString -> RelOp
parseRelOp op = case op of
  "<" -> LessThan
  ">" -> GreaterThan
  "<=" -> LessEqual
  ">=" -> GreaterEqual

parseEqOp :: ByteString -> EqOp
parseEqOp op = case op of
  "==" -> Equal
  "!=" -> NotEqual

parseCondOp :: ByteString -> CondOp
parseCondOp op = case op of
  "||" -> OR
  "&&" -> AND

parseNegOp :: ByteString -> NegOp
parseNegOp op = case op of
  "-" -> Neg

parseNotOp :: ByteString -> NotOp
parseNotOp op = case op of
  "!" -> Not

parseAssignOp :: ByteString -> AssignOp
parseAssignOp s = case s of
  "+=" -> IncAssign
  "-=" -> DecAssign
  "=" -> EqlAssign
  "++" -> PlusPlus
  "--" -> MinusMinus

-- auxiliary data types
data Location = Location
  { name :: Name,
    idx :: Maybe Expr,
    variableDef :: Either Argument FieldDecl
  }

instance Show Location where
  show (Location nm idx _) = printf "Location {name=%s, idx=%s}" (B.toString nm) (show idx)

data Assignment = Assignment
  { location :: WithType Location,
    op :: AssignOp,
    expr :: Maybe (WithType Expr)
  }
  deriving (Show)

data MethodCall = MethodCall
  { name :: Name,
    args :: [WithType Expr]
  }
  deriving (Show)

-- ir nodes
data IRRoot = IRRoot
  { imports :: [ImportDecl],
    vars :: [FieldDecl],
    methods :: [MethodDecl]
  }
  deriving (Show)

data ImportDecl = ImportDecl {name :: Name}
  deriving (Show)

data FieldDecl = FieldDecl
  { name :: Name,
    tpe :: Type,
    size :: Maybe Int64
  }
  deriving (Show)

data Argument = Argument
  { name :: Name,
    tpe :: Type
  }
  deriving (Show, Eq)

data MethodSig = MethodSig
  { name :: Name,
    tpe :: (Maybe Type),
    args :: [Argument]
  }
  deriving (Show, Eq)

data MethodDecl = MethodDecl
  { sig :: MethodSig,
    block :: Block
  }
  deriving (Show)

data Statement
  = AssignStmt {assign :: Assignment}
  | IfStmt {pred :: WithType Expr, ifBlock :: Block, elseBlock :: Maybe Block}
  | ForStmt
      { counter :: Name,
        initCounter :: WithType Expr,
        pred :: WithType Expr,
        update :: Assignment,
        block :: Block
      }
  | WhileStmt {pred :: WithType Expr, block :: Block}
  | ReturnStmt {expr :: (Maybe (WithType Expr))}
  | MethodCallStmt {methodCall :: MethodCall}
  | BreakStmt
  | ContinueStmt
  deriving (Show)

data Expr
  = LocationExpr {location :: Location}
  | MethodCallExpr {methodCall :: MethodCall}
  | ExternCallExpr {name :: Name, args :: [WithType Expr]}
  | IntLiteralExpr {intVal :: Int64}
  | BoolLiteralExpr {boolVal :: Bool}
  | CharLiteralExpr {charVal :: Char}
  | StringLiteralExpr {strVal :: ByteString}
  | ArithOpExpr {arithOp :: ArithOp, lhs :: WithType Expr, rhs :: WithType Expr}
  | RelOpExpr {relOp :: RelOp, lhs :: WithType Expr, rhs :: WithType Expr}
  | CondOpExpr {condOp :: CondOp, lhs :: WithType Expr, rhs :: WithType Expr}
  | EqOpExpr {eqOp :: EqOp, lhs :: WithType Expr, rhs :: WithType Expr}
  | NegOpExpr {negOp :: NegOp, expr :: WithType Expr}
  | NotOpExpr {notOp :: NotOp, expr :: WithType Expr}
  | ChoiceOpExpr {choiceOp :: ChoiceOp, expr1 :: WithType Expr, expr2 :: WithType Expr, expr3 :: WithType Expr}
  | LengthExpr {name :: Name}
  deriving (Show)

data WithType a = WithType {ele :: a, tpe :: Type}
  deriving (Show)

data Block = Block
  { vars :: [FieldDecl],
    stats :: [Statement],
    blockID :: ScopeID
  }
  deriving (Show)
