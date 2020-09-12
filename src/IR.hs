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
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | Equal
  | NotEqual
  | OR
  | AND
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
  | PlusPlus
  | MinusMinus
  deriving (Show)

data Type =
  IntType
  | BoolType
  deriving (Show)

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

data IRNode = IRRoot [IRNode] [IRNode] [IRNode]
            -- declarations
            | ImportDecl Name
            | FieldDecl Type Name (Maybe Int)
            | MethodDecl (Maybe Type) Name [(Name, Type)] IRNode {- block -}
            -- statement nodes
            | AssignStmt AssignOp IRNode {- location -} (Maybe IRNode) {- expr -}
            | IfStmt IRNode {- expr -} IRNode {- if stmts -} (Maybe IRNode) {- else stmts -}
            | ForStmt Name {- counter -}
              IRNode {- counter expr -} IRNode {- pred expr -}
              IRNode {- update stmt -} IRNode {- block -}
            | WhileStmt IRNode {- pred expr -} IRNode {- block -}
            | ReturnStmt (Maybe IRNode)
            | BreakStmt
            | ContinueStmt
            -- experssion nodes
            | LocationExpr Name (Maybe IRNode)
            | MethodCallExpr Name [IRNode]
            | ExternCallExpr Name [IRNode]
            | IntLiteralExpr Int
            | BoolLiteralExpr Bool
            | CharLiteralExpr Char
            | StringLiteralExpr ByteString
            | BinaryOpExpr BinaryOp IRNode IRNode
            | UnaryOpExpr UnaryOp IRNode
            | LengthExpr Name
            | TernaryOpExpr TernaryOp IRNode {- pred expr -} IRNode IRNode
            -- block scope
            | Block [IRNode] [IRNode]
            deriving (Show)

----------------------------------------------------------------------
-- Reformat the parser tree into an IR tree
-- NOTE: Not sure if this conversion is necessary. But do it for now.
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
generate (P.Program imports fields methods) = IRRoot
  (irgenImportDecl <$> imports)
  (concat $ irgenFieldDecl <$> fields)
  (irgenMethodDecl <$> methods)

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

irgenMethodDecl :: P.MethodDecl -> IRNode
irgenMethodDecl (P.MethodDecl id returnType arguments block) =
  MethodDecl (irgenType <$> returnType) id args (irgenBlock block)
  where
    args = map (\(P.Argument id tpe) -> (id, irgenType tpe)) arguments

irgenBlock :: P.Block -> IRNode
irgenBlock (P.Block fieldDecls statements) = Block fields stmts
  where
    fields = concat $ irgenFieldDecl <$> fieldDecls
    stmts = irgenStmt <$> statements

irgenLocation :: P.Location -> IRNode
irgenLocation (P.ScalarLocation id) = LocationExpr id Nothing
irgenLocation (P.VectorLocation id expr) = LocationExpr id (Just $ irgenExpr expr)

irgenAssignExpr :: P.Location -> P.AssignExpr -> IRNode
irgenAssignExpr loc expr = AssignStmt op' (irgenLocation loc) expr'
  where (op', expr') = case expr of
          (P.AssignExpr op expr) -> (parseAssignOp op, Just $ irgenExpr expr)
          (P.IncrementExpr op)   -> (parseAssignOp op, Nothing)

irgenStmt :: P.Statement -> IRNode
irgenStmt (P.AssignStatement loc expr) = irgenAssignExpr loc expr
irgenStmt (P.MethodCallStatement (P.MethodCall method args)) = MethodCallExpr method (irgenImportArg <$> args)
irgenStmt (P.IfStatement expr block) = IfStmt (irgenExpr expr) (irgenBlock block) Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = IfStmt (irgenExpr expr) (irgenBlock ifBlock) (Just $ irgenBlock elseBlock)
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) =
  ForStmt counter (irgenExpr counterExpr) (irgenExpr predExpr) (irgenAssignExpr loc expr) (irgenBlock block)
irgenStmt (P.WhileStatement expr block) = WhileStmt (irgenExpr expr) (irgenBlock block)
irgenStmt (P.ReturnExprStatement expr) = ReturnStmt $ Just $ irgenExpr expr
irgenStmt P.ReturnVoidStatement = ReturnStmt Nothing
irgenStmt P.BreakStatement = BreakStmt
irgenStmt P.ContinueStatement = ContinueStmt

irgenExpr :: P.Expr -> IRNode
irgenExpr (P.LocationExpr loc)    = irgenLocation loc
irgenExpr (P.MethodCallExpr (P.MethodCall method args)) = MethodCallExpr method (irgenImportArg <$> args)
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

irgenImportArg :: P.ImportArg -> IRNode
irgenImportArg (P.ExprImportArg expr)  = irgenExpr expr
irgenImportArg (P.StringImportArg arg) = StringLiteralExpr arg
