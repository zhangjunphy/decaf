-- IR -- Decaf Intermidiate code
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
{-# LANGUAGE OverloadedStrings #-}

module IR where

import Data.Int (Int64)
import Data.Text (Text)

type Name = Text

type Label = Text

type Index = Int64

type ScopeID = Int

type SymbolID = Int

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

parseArithOp :: Text -> ArithOp
parseArithOp op = case op of
  "+" -> Plus
  "-" -> Minus
  "*" -> Multiply
  "/" -> Division
  "%" -> Modulo

parseRelOp :: Text -> RelOp
parseRelOp op = case op of
  "<" -> LessThan
  ">" -> GreaterThan
  "<=" -> LessEqual
  ">=" -> GreaterEqual

parseEqOp :: Text -> EqOp
parseEqOp op = case op of
  "==" -> Equal
  "!=" -> NotEqual

parseCondOp :: Text -> CondOp
parseCondOp op = case op of
  "||" -> OR
  "&&" -> AND

parseNegOp :: Text -> NegOp
parseNegOp op = case op of
  "-" -> Neg

parseNotOp :: Text -> NotOp
parseNotOp op = case op of
  "!" -> Not

parseAssignOp :: Text -> AssignOp
parseAssignOp s = case s of
  "+=" -> IncAssign
  "-=" -> DecAssign
  "=" -> EqlAssign
  "++" -> PlusPlus
  "--" -> MinusMinus

{-
-- SSA instructions
-}

data Literal = IntLiteral { intVal :: Int64 }
             | BoolLiteral { boolVal :: Bool }
             | StringLiteral {stringVal :: Text}
             deriving (Show)

data Address = Variable { sym :: Name, name :: Name, tpe :: Type }
             | Constant { lit :: Literal }
             | Temporal { sym :: Name, tpe :: Type }
             deriving (Show)

typeOfLiteral :: Literal -> Type
typeOfLiteral (IntLiteral _) = IntType
typeOfLiteral (BoolLiteral _) = IntType
typeOfLiteral (StringLiteral _) = IntType

typeOf :: Address -> Type
typeOf (Variable _ _ tpe) = tpe
typeOf (Constant lit) = typeOfLiteral lit
typeOf (Temporal _ tpe) = tpe

data IRInstruction
  = Arithmetic {target :: Address, arithOp :: ArithOp, lhs :: Address, rhs :: Address}
  | Relational {target :: Address, relOp :: RelOp, lhs :: Address, rhs :: Address}
  | Condition {target :: Address, condOp :: CondOp, lhs :: Address, rhs :: Address}
  | Equality {target :: Address, eqOp :: EqOp, lhs :: Address, rhs :: Address}
  | UnaryMinus {target :: Address, source :: Address}
  | Negate {target :: Address, source :: Address}
  | ScalarCopy {target :: Address, source :: Address}
  | ArrayToScalarCopy {target :: Address, source :: Address, sourceIndex :: Address}
  | ScalarToArrayCopy {target :: Address, source :: Address, targetIndex :: Address}
  | UnconditionalJump {label :: Label}
  | ConditionalJump {pred :: Address, label :: Label}
  | ProcedureCall {target :: Address, method :: Name, params :: [Address]}
  | Return {value :: Maybe Address}
  deriving (Show)
