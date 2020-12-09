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
import Data.List (intercalate)
import Formatting

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
  | Equal
  | NotEqual
  deriving (Eq)

instance Show RelOp where
  show op
    | op == LessThan = "<"
    | op == GreaterThan = ">"
    | op == LessEqual = "<="
    | op == GreaterEqual = ">="
    | op == Equal = "=="
    | op == NotEqual = "!="

data ArithOp
  = Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  deriving (Eq)

instance Show ArithOp where
  show op
    | op == Plus = "+"
    | op == Minus = "-"
    | op == Multiply = "*"
    | op == Division = "/"
    | op == Modulo = "%"

data LogicalOp
  = OR
  | AND
  deriving (Eq)

instance Show LogicalOp where
  show op
    | op == OR = "||"
    | op == AND = "&&"

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

parseEqOp :: Text -> RelOp
parseEqOp op = case op of
  "==" -> Equal
  "!=" -> NotEqual

parseCondOp :: Text -> LogicalOp
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

data Literal
  = IntLiteral {intVal :: Int64}
  | BoolLiteral {boolVal :: Bool}
  | CharLiteral {charVal :: Char}
  | StringLiteral {stringVal :: Text}

instance Show Literal where
  show (IntLiteral val) = show val
  show (BoolLiteral val) = show val
  show (CharLiteral val) = show val
  show (StringLiteral val) = show val

data Address
  = Variable {sym :: Int, name :: Name, tpe :: Type}
  | Constant {lit :: Literal}
  | Temporal {sym :: Int, tpe :: Type}

instance Show Address where
  show (Variable sym name _) = formatToString (stext%"_"%int) name sym
  show (Constant lit) = show lit
  show (Temporal sym _) = formatToString ("temp_"%int) sym

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
  | Logical {target :: Address, condOp :: LogicalOp, lhs :: Address, rhs :: Address}
  | UnaryMinus {target :: Address, source :: Address}
  | Negation {target :: Address, source :: Address}
  | ScalarCopy {target :: Address, source :: Address}
  | ArrayToScalarCopy {target :: Address, source :: Address, sourceIndex :: Address}
  | ScalarToArrayCopy {target :: Address, source :: Address, targetIndex :: Address}
  | UnconditionalJump {label :: Label}
  | ConditionalJump {pred :: Address, label :: Label}
  | ProcedureCall {returns :: Maybe Address, method :: Name, params :: [Address]}

instance Show IRInstruction where
  show (Arithmetic target op lhs rhs) =
    formatToString (shown%" = "%shown%" "%shown%" "%shown) target lhs op rhs
  show (Relational target op lhs rhs) =
    formatToString (shown%" = "%shown%" "%shown%" "%shown) target lhs op rhs
  show (Logical target op lhs rhs) =
    formatToString (shown%" = "%shown%" "%shown%" "%shown) target lhs op rhs
  show (UnaryMinus target src) =
    formatToString (shown%" = - "%shown) target src
  show (Negation target src) =
    formatToString (shown%" = ! "%shown) target src
  show (ScalarCopy target src) =
    formatToString (shown%" = "%shown) target src
  show (ArrayToScalarCopy target source idx) =
    formatToString (shown%" = "%shown%"["%shown%"]") target source idx
  show (ScalarToArrayCopy target source idx) =
    formatToString (shown%"["%shown%"] = "%shown) target idx source
  show (UnconditionalJump label) =
    formatToString ("jump "%shown) label
  show (ConditionalJump pred label) =
    formatToString ("jumpif "%shown%" "%shown) pred label
  show (ProcedureCall ret method params) = case ret of
    Nothing -> formatToString ("call "%shown%" "%string) method (intercalate "," (show <$> params))
    Just ret' -> formatToString (shown%"= call "%shown%" "%string) ret' method (intercalate "," (show <$> params))
