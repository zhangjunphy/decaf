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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module IR ( generate
          , Location(..), Assignment(..), MethodCall(..)
          , IRRoot(..), ImportDecl(..), FieldDecl(..), MethodDecl(..)
          , Statement(..), Expr(..), Block(..)
          , Name
          , BinaryOp
          , UnaryOp
          , TernaryOp
          , AssignOp
          , Type
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

type Name = ByteString

-- operators
data BinaryOp =
  Plus
  | Minus
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | Equal
  | NotEqual
  | OR
  | AND
  deriving (Show, Eq)

data UnaryOp =
  Negate
  | Negative
  deriving (Show, Eq)

data TernaryOp =
  Choice
  deriving (Show, Eq)

data AssignOp =
  EqlAssign
  | IncAssign
  | DecAssign
  | PlusPlus
  | MinusMinus
  deriving (Show, Eq)

data Type =
  IntType
  | BoolType
  deriving (Show, Eq)

parseBinaryOp :: ByteString -> BinaryOp
parseBinaryOp op = case op of
  "+"  -> Plus
  "-"  -> Minus
  "<"  -> LessThan
  ">"  -> GreaterThan
  "<=" -> LessEqual
  ">=" -> GreaterEqual
  "==" -> Equal
  "!=" -> NotEqual
  "||" -> OR
  "&&" -> AND

parseUnaryOp :: ByteString -> UnaryOp
parseUnaryOp op = case op of
  "-" -> Negative
  "!" -> Negate

parseAssignOp :: ByteString -> AssignOp
parseAssignOp s = case s of
  "+=" -> IncAssign
  "-=" -> DecAssign
  "="  -> EqlAssign
  "++" -> PlusPlus
  "--" -> MinusMinus

-- auxiliary data types
data Location = Location { name :: Name
                         , idx  :: Maybe Expr
                         } deriving Show

data Assignment = Assignment { location :: Location
                             , op       :: AssignOp
                             , expr     :: Maybe Expr
                             } deriving Show

data MethodCall = MethodCall { name :: Name
                             , args :: [Expr]
                             } deriving Show

-- ir nodes
data IRRoot = IRRoot { imports :: [ImportDecl]
                     , vars    :: [FieldDecl]
                     , methods :: [MethodDecl]
                     } deriving Show

data ImportDecl = ImportDecl { name :: Name }
  deriving Show

data FieldDecl = FieldDecl { name :: Name
                           , tpe  :: Type
                           , size :: Maybe Int
                           } deriving Show

data MethodDecl = MethodDecl { name  :: Name
                             , tpe   :: (Maybe Type)
                             , args  :: [(Name, Type)]
                             , block :: Block
                             } deriving Show

data Statement = AssignStmt { assign :: Assignment }
               | IfStmt { pred :: Expr, ifBlock :: Block, elseBlock :: Maybe Block}
               | ForStmt { counter     :: Name
                         , initCounter :: Expr
                         , pred        :: Expr
                         , update      :: Assignment
                         , block       :: Block}
               | WhileStmt { pred :: Expr, block :: Block}
               | ReturnStmt { expr :: (Maybe Expr) }
               | MethodCallStmt { methodCall :: MethodCall }
               | BreakStmt
               | ContinueStmt
               deriving Show

data Expr = LocationExpr { location :: Location }
          | MethodCallExpr { methodCall :: MethodCall }
          | ExternCallExpr { name :: Name, args :: [Expr] }
          | IntLiteralExpr { intVal :: Int }
          | BoolLiteralExpr { boolVal :: Bool }
          | CharLiteralExpr { charVal :: Char }
          | StringLiteralExpr { strVal :: ByteString }
          | BinaryOpExpr { binaryOp :: BinaryOp, lhs :: Expr, rhs :: Expr }
          | UnaryOpExpr { uanryOp :: UnaryOp, expr :: Expr }
          | TernaryOpExpr { tenaryOp :: TernaryOp, expr1 :: Expr, expr2 :: Expr, expr3 :: Expr }
          | LengthExpr { name :: Name }
          deriving Show

data Block = Block { vars  :: [FieldDecl]
                   , stats :: [Statement]
                   } deriving Show

----------------------------------------------------------------------
-- Convert the parser tree into an IR tree
----------------------------------------------------------------------

generate :: P.Program -> IRRoot
generate (P.Program imports fields methods) = IRRoot
  (irgenImportDecl <$> imports)
  (concat $ irgenFieldDecl <$> fields)
  (irgenMethodDecl <$> methods)

irgenType :: P.Type -> Type
irgenType P.IntType  = IntType
irgenType P.BoolType = BoolType

irgenImportDecl :: P.ImportDecl -> ImportDecl
irgenImportDecl (P.ImportDecl id) = ImportDecl id

irgenFieldDecl :: P.FieldDecl -> [FieldDecl]
irgenFieldDecl (P.FieldDecl tpe elems) =
  flip fmap elems $ \case
      (P.ScalarField id)
        -> FieldDecl id (irgenType tpe) Nothing
      (P.VectorField id size)
        -> FieldDecl id (irgenType tpe) (Just sz)
           where sz = read $ B.toString size

irgenMethodDecl :: P.MethodDecl -> MethodDecl
irgenMethodDecl (P.MethodDecl id returnType arguments block) =
  MethodDecl id (irgenType <$> returnType) args (irgenBlock block)
  where
    args = map (\(P.Argument id tpe) -> (id, irgenType tpe)) arguments

irgenBlock :: P.Block -> Block
irgenBlock (P.Block fieldDecls statements) = Block fields stmts
  where
    fields = concat $ irgenFieldDecl <$> fieldDecls
    stmts = irgenStmt <$> statements

irgenLocation :: P.Location -> Location
irgenLocation (P.ScalarLocation id)      = Location id Nothing
irgenLocation (P.VectorLocation id expr) = Location id (Just $ irgenExpr expr)

irgenAssign :: P.Location -> P.AssignExpr -> Assignment
irgenAssign loc expr = Assignment (irgenLocation loc) op' expr'
  where (op', expr') = case expr of
          (P.AssignExpr op expr) -> (parseAssignOp op, Just $ irgenExpr expr)
          (P.IncrementExpr op)   -> (parseAssignOp op, Nothing)

irgenStmt :: P.Statement -> Statement
irgenStmt (P.AssignStatement loc expr) = AssignStmt $ irgenAssign loc expr
irgenStmt (P.MethodCallStatement (P.MethodCall method args)) = MethodCallStmt $
  MethodCall method (irgenImportArg <$> args)
irgenStmt (P.IfStatement expr block) = IfStmt (irgenExpr expr) (irgenBlock block) Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = IfStmt (irgenExpr expr) (irgenBlock ifBlock) (Just $ irgenBlock elseBlock)
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) =
  ForStmt counter (irgenExpr counterExpr) (irgenExpr predExpr) (irgenAssign loc expr) (irgenBlock block)
irgenStmt (P.WhileStatement expr block) = WhileStmt (irgenExpr expr) (irgenBlock block)
irgenStmt (P.ReturnExprStatement expr) = ReturnStmt $ Just $ irgenExpr expr
irgenStmt P.ReturnVoidStatement = ReturnStmt Nothing
irgenStmt P.BreakStatement = BreakStmt
irgenStmt P.ContinueStatement = ContinueStmt

irgenExpr :: P.Expr -> Expr
irgenExpr (P.LocationExpr loc)    = LocationExpr $ irgenLocation loc
irgenExpr (P.MethodCallExpr (P.MethodCall method args)) = MethodCallExpr $
  MethodCall method (irgenImportArg <$> args)
irgenExpr (P.IntLiteralExpr i) = IntLiteralExpr $ read $ B.toString i
irgenExpr (P.BoolLiteralExpr b) = BoolLiteralExpr $ read $ B.toString b
irgenExpr (P.CharLiteralExpr c) = CharLiteralExpr $ read $ B.toString c
irgenExpr (P.LenExpr id) = LengthExpr id
irgenExpr (P.ArithOpExpr op l r) = BinaryOpExpr (parseBinaryOp op) (irgenExpr l) (irgenExpr r)
irgenExpr (P.RelOpExpr op l r) = BinaryOpExpr (parseBinaryOp op) (irgenExpr l) (irgenExpr r)
irgenExpr (P.EqOpExpr op l r) = BinaryOpExpr (parseBinaryOp op) (irgenExpr l) (irgenExpr r)
irgenExpr (P.CondOpExpr op l r) = BinaryOpExpr (parseBinaryOp op) (irgenExpr l) (irgenExpr r)
irgenExpr (P.NegativeExpr expr) = UnaryOpExpr Negative (irgenExpr expr)
irgenExpr (P.NegateExpr expr) = UnaryOpExpr Negate (irgenExpr expr)
irgenExpr (P.ParenExpr expr) = irgenExpr expr
irgenExpr (P.ChoiceExpr pred l r) = TernaryOpExpr Choice (irgenExpr pred) (irgenExpr l) (irgenExpr r)

irgenImportArg :: P.ImportArg -> Expr
irgenImportArg (P.ExprImportArg expr)  = irgenExpr expr
irgenImportArg (P.StringImportArg arg) = StringLiteralExpr arg
