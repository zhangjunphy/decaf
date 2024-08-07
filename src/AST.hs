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

-- AST -- AST after type checking and clean up
module AST where

import Control.Lens (view)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Text (Text)
import Formatting
import Data.Generics.Labels
import GHC.Generics (Generic)
import Text.Printf (printf)
import Types
import Util.SourceLoc qualified as SL

-- operators
data RelOp
  = LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  deriving (Eq)

instance Show RelOp where
  show LessThan = "<"
  show GreaterThan = ">"
  show LessEqual = "<="
  show GreaterEqual = ">="

data ArithOp
  = Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  deriving (Eq)

instance Show ArithOp where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Division = "/"
  show Modulo = "%"

data EqOp
  = Equal
  | NotEqual
  deriving (Eq)

instance Show EqOp where
  show Equal = "=="
  show NotEqual = "!="

data CondOp
  = Or
  | And
  deriving (Eq)

instance Show CondOp where
  show Or = "||"
  show And = "&&"

data NegOp
  = Neg
  deriving (Eq)

instance Show NegOp where
  show Neg = "-"

data NotOp
  = Not
  deriving (Eq)

instance Show NotOp where
  show Not = "!"

data ChoiceOp
  = Choice
  deriving (Show, Eq)

data AssignOp
  = EqlAssign
  | IncAssign
  | DecAssign
  | PlusPlus
  | MinusMinus
  deriving (Eq)

instance Show AssignOp where
  show EqlAssign = "="
  show IncAssign = "+="
  show DecAssign = "-="
  show PlusPlus = "++"
  show MinusMinus = "--"

data Type
  = Void
  | BoolType
  | CharType
  | IntType
  | StringType
  | ArrayType !Type !Int64
  | Ptr !Type
  deriving (Eq)

instance Show Type where
  show Void = "void"
  show BoolType = "bool"
  show CharType = "char"
  show IntType = "int"
  show StringType = "string"
  show (ArrayType tpe size) = formatToString (shown % "x" % shown) size tpe
  show (Ptr tpe) = formatToString ("ptr" %+ shown) tpe

dataSize :: Type -> Maybe Int64
dataSize Void = Nothing
dataSize IntType = Just 8
dataSize BoolType = Just 1
dataSize StringType = Just 8
dataSize (ArrayType tpe size) = dataSize tpe <&> \s -> s * size
dataSize (Ptr _) = Just 8

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
  "||" -> Or
  "&&" -> And

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
  { name :: !Name,
    idx :: !(Maybe Expr),
    variableDef :: !(Either Argument FieldDecl),
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic)

typeOfDef :: Either Argument FieldDecl -> Type
typeOfDef (Left Argument {tpe = tpe}) = tpe
typeOfDef (Right FieldDecl {tpe = tpe}) = tpe

instance Show Location where
  show Location {name = nm, idx = idx} = printf "Location {name=%s, idx=%s}" nm (show idx)

data Assignment = Assignment
  { location :: !Location,
    op :: !AssignOp,
    expr :: !(Maybe Expr),
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data MethodCall = MethodCall
  { name :: !Name,
    args :: ![Expr],
    loc :: !SL.Range
  }
  deriving (Generic, Show)

-- AST nodes
data ASTRoot = ASTRoot
  { imports :: ![ImportDecl],
    vars :: ![FieldDecl],
    methods :: ![MethodDecl]
  }
  deriving (Generic, Show)

data ImportDecl = ImportDecl
  { name :: !Name,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data FieldDecl = FieldDecl
  { name :: !Name,
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data Argument = Argument
  { name :: !Name,
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data MethodSig = MethodSig
  { name :: !Name,
    tpe :: !(Maybe Type),
    args :: ![Argument]
  }
  deriving (Generic, Show)

mangle :: MethodSig -> Text
mangle (MethodSig name _ args) =
  sformat (stext % "@" % intercalated "@" shown) name (args <&> view #tpe)

data MethodDecl = MethodDecl
  { sig :: !MethodSig,
    block :: !Block,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data Statement = Statement
  { statement_ :: !Statement_,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data Statement_
  = AssignStmt {assign :: !Assignment}
  | IfStmt {pred :: !Expr, ifBlock :: !Block, elseBlock :: !(Maybe Block)}
  | ForStmt {init :: !(Maybe Assignment), pred :: !Expr, update :: !(Maybe Assignment), block :: !Block}
  | ReturnStmt {expr :: !(Maybe Expr)}
  | MethodCallStmt {methodCall :: !MethodCall}
  | BreakStmt
  | ContinueStmt
  deriving (Generic, Show)

data Expr = Expr
  { expr_ :: !Expr_,
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic, Show)

data Expr_
  = LocationExpr {location :: !Location}
  | MethodCallExpr {methodCall :: !MethodCall}
  | ExternCallExpr {name :: !Name, args :: ![Expr]}
  | IntLiteralExpr {intVal :: !Int64}
  | BoolLiteralExpr {boolVal :: !Bool}
  | CharLiteralExpr {charVal :: !Char}
  | StringLiteralExpr {strVal :: !Text}
  | ArithOpExpr {arithOp :: !ArithOp, lhs :: !Expr, rhs :: !Expr}
  | RelOpExpr {relOp :: !RelOp, lhs :: !Expr, rhs :: !Expr}
  | CondOpExpr {condOp :: !CondOp, lhs :: !Expr, rhs :: !Expr}
  | EqOpExpr {eqOp :: !EqOp, lhs :: !Expr, rhs :: !Expr}
  | NegOpExpr {negOp :: !NegOp, expr :: !Expr}
  | NotOpExpr {notOp :: !NotOp, expr :: !Expr}
  | ChoiceOpExpr {choiceOp :: !ChoiceOp, expr1 :: !Expr, expr2 :: !Expr, expr3 :: !Expr}
  | LengthExpr {name :: !Name}
  deriving (Generic, Show)

data Typed a = Typed {ele :: !a, tpe :: !Type}
  deriving (Generic, Show)

data Block = Block
  { vars :: ![FieldDecl],
    stmts :: ![Statement],
    blockID :: !ScopeID
  }
  deriving (Generic, Show)
