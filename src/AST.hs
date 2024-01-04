-- AST -- AST after type checking and clean up
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
module AST where

import GHC.Generics (Generic)

import Data.Int (Int64)
import Data.Text (Text)
import Text.Printf (printf)

import qualified SourceLoc as SL

type Name = Text

type Index = Int64

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
  | ArrayType Type Int64
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

-- auxiliary data types
data Location = Location
  { name :: Name,
    idx :: Maybe Expr,
    variableDef :: Either Argument FieldDecl
  }

typeOfDef :: Either Argument FieldDecl -> Type
typeOfDef (Left (Argument _ tpe)) = tpe
typeOfDef (Right (FieldDecl _ tpe)) = tpe

instance Show Location where
  show (Location nm idx _) = printf "Location {name=%s, idx=%s}" nm (show idx)

data Assignment = Assignment
  { location :: Typed Location,
    op :: AssignOp,
    expr :: Maybe (SL.Located (Typed Expr))
  }
  deriving (Generic, Show)

data MethodCall = MethodCall
  { name :: Name,
    args :: [SL.Located (Typed Expr)]
  }
  deriving (Generic, Show)

-- AST nodes
data ASTRoot = ASTRoot
  { imports :: [SL.Located ImportDecl],
    vars :: [SL.Located FieldDecl],
    methods :: [SL.Located MethodDecl]
  }
  deriving (Generic, Show)

data ImportDecl = ImportDecl {name :: Name}
  deriving (Generic, Show)

data FieldDecl = FieldDecl
  { name :: Name,
    tpe :: Type
  }
  deriving (Generic, Show)

data Argument = Argument
  { name :: Name,
    tpe :: Type
  }
  deriving (Generic, Show)

data MethodSig = MethodSig
  { name :: Name,
    tpe :: Maybe Type,
    args :: [SL.Located Argument]
  }
  deriving (Generic, Show)

data MethodDecl = MethodDecl
  { sig :: MethodSig,
    block :: Block
  }
  deriving (Generic, Show)

data Statement
  = AssignStmt {assign :: Assignment}
  | IfStmt {pred :: SL.Located (Typed Expr), ifBlock :: Block, elseBlock :: Maybe Block}
  | ForStmt
      { counter :: Name,
        initCounter :: SL.Located (Typed Expr),
        pred :: SL.Located (Typed Expr),
        update :: Assignment,
        block :: Block
      }
  | WhileStmt {pred :: SL.Located (Typed Expr), block :: Block}
  | ReturnStmt {expr :: Maybe (SL.Located (Typed Expr))}
  | MethodCallStmt {methodCall :: MethodCall}
  | BreakStmt
  | ContinueStmt
  | VarDeclStmt {field :: FieldDecl} -- TODO: Decide if we are going to use this.
  deriving (Generic, Show)

data Expr
  = LocationExpr {location :: Location}
  | MethodCallExpr {methodCall :: MethodCall}
  | ExternCallExpr {name :: Name, args :: [SL.Located (Typed Expr)]}
  | IntLiteralExpr {intVal :: Int64}
  | BoolLiteralExpr {boolVal :: Bool}
  | CharLiteralExpr {charVal :: Char}
  | StringLiteralExpr {strVal :: Text}
  | ArithOpExpr {arithOp :: ArithOp, lhs :: SL.Located (Typed Expr), rhs :: SL.Located (Typed Expr)}
  | RelOpExpr {relOp :: RelOp, lhs :: SL.Located (Typed Expr), rhs :: SL.Located (Typed Expr)}
  | CondOpExpr {condOp :: CondOp, lhs :: SL.Located (Typed Expr), rhs :: SL.Located (Typed Expr)}
  | EqOpExpr {eqOp :: EqOp, lhs :: SL.Located (Typed Expr), rhs :: SL.Located (Typed Expr)}
  | NegOpExpr {negOp :: NegOp, expr :: SL.Located (Typed Expr)}
  | NotOpExpr {notOp :: NotOp, expr :: SL.Located (Typed Expr)}
  | ChoiceOpExpr {choiceOp :: ChoiceOp, expr1 :: SL.Located (Typed Expr), expr2 :: SL.Located (Typed Expr), expr3 :: SL.Located (Typed Expr)}
  | LengthExpr {name :: Name}
  deriving (Generic, Show)

data Typed a = Typed {ele :: a, tpe :: Type}
  deriving (Generic, Show)

data Block = Block
  { vars :: [SL.Located FieldDecl],
    stmts :: [SL.Located Statement],
    blockID :: ScopeID
  }
  deriving (Generic, Show)
