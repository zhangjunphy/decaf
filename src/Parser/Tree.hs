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

module Parser.Tree where

import Data.Text (Text)
import Util.SourceLoc as SL

data Program = Program
  { importDecls :: ![SL.Located ImportDecl],
    fieldDecls :: ![SL.Located FieldDecl],
    methodDecls :: ![SL.Located MethodDecl]
  }
  deriving (Show)

data ImportDecl = ImportDecl {importId :: !Text}
  deriving (Show)

data FieldDecl = FieldDecl
  { fieldType :: !Type,
    elems :: ![SL.Located FieldElem]
  }
  deriving (Show)

data FieldElem
  = ScalarField {fieldId :: !Text}
  | VectorField {fieldId :: !Text, size :: !Text}
  deriving (Show)

data Type = IntType | BoolType
  deriving (Show)

data MethodDecl = MethodDecl
  { methodId :: !Text,
    returnType :: !(Maybe Type),
    arguments :: ![SL.Located Argument],
    block :: !Block
  }
  deriving (Show)

data Argument = Argument
  { argumentId :: !Text,
    argumentType :: !Type
  }
  deriving (Show)

data Block = Block
  { blockFieldDecls :: ![SL.Located FieldDecl],
    blockStatements :: ![SL.Located Statement]
  }
  deriving (Show)

data Statement
  = AssignStatement {assignLocation :: !Location, assignExpr :: !AssignExpr}
  | MethodCallStatement {methodCallStatement :: !MethodCall}
  | IfStatement {ifExpr :: !(SL.Located Expr), ifBlock :: !Block}
  | IfElseStatement {ifExpr :: !(SL.Located Expr), ifBlock :: !Block, elseBlock :: !Block}
  | ForStatement
      { counterId :: !Text,
        counterExpr :: !(SL.Located Expr),
        forPredExpr :: !(SL.Located Expr),
        counterUpdate :: !CounterUpdate,
        forBlock :: !Block
      }
  | WhileStatement {whileExpr :: !(SL.Located Expr), whileBlock :: !Block}
  | ReturnVoidStatement
  | ReturnExprStatement {returnExpr :: !(SL.Located Expr)}
  | BreakStatement
  | ContinueStatement
  | ErrorStatement
  deriving (Show)

data Location
  = ScalarLocation {locationId :: !Text}
  | VectorLocation {locationId :: !Text, arrayIndexExpr :: !(SL.Located Expr)}
  deriving (Show)

data AssignExpr
  = AssignExpr {assignOp :: !Text, assignSourceExpr :: !(SL.Located Expr)}
  | IncrementExpr {incrementOp :: !Text}
  deriving (Show)

data MethodCall = MethodCall {methodName :: !Text, importArguments :: ![SL.Located ImportArg]}
  deriving (Show)

data ImportArg
  = ExprImportArg {argumentExpr :: !(SL.Located Expr)}
  | StringImportArg {argumentString :: !Text}
  deriving (Show)

data CounterUpdate = CounterUpdate {counterLocation :: !Location, updateExpr :: !AssignExpr}
  deriving (Show)

data Expr
  = LocationExpr {location :: !Location}
  | MethodCallExpr {methodCallExpr :: !MethodCall}
  | IntLiteralExpr {intLiteral :: !Text}
  | CharLiteralExpr {charLiteral :: !Text}
  | BoolLiteralExpr {boolLiteral :: !Text}
  | LenExpr {lenId :: !Text}
  | ArithOpExpr {arithOp :: !Text, lExpr :: !(SL.Located Expr), rExpr :: !(SL.Located Expr)}
  | RelOpExpr {relOp :: !Text, lExpr :: !(SL.Located Expr), rExpr :: !(SL.Located Expr)}
  | EqOpExpr {eqOp :: !Text, lExpr :: !(SL.Located Expr), rExpr :: !(SL.Located Expr)}
  | CondOpExpr {condOp :: !Text, lExpr :: !(SL.Located Expr), rExpr :: !(SL.Located Expr)}
  | NegativeExpr {negativeExpr :: !(SL.Located Expr)}
  | NegateExpr {negateExpr :: !(SL.Located Expr)}
  | ParenExpr {parenExpr :: !(SL.Located Expr)}
  | ChoiceExpr {choicePredExpr :: !(SL.Located Expr), lExpr :: !(SL.Located Expr), rExpr :: !(SL.Located Expr)}
  deriving (Show)
