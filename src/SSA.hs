-- SSA -- SSA Form Low Level IR
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

module SSA where

import Control.Monad.State
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import qualified IR
import qualified Semantic as SE

type BlockLabel = Int

data Type
  = IntType
  | BoolType
  | CharType
  | ArrayType Type

convertIRType :: IR.Type -> Type
convertIRType t = _

data Imm
  = IntImm Int64
  | BoolImm Bool
  | CharImm Char

data Identifier = Identifier
  { id :: Int,
    tpe :: Type
  }

data Operand
  = Operand Identifier
  | Imm

data Block = Block
  { label :: BlockLabel,
    arguments :: [Identifier],
    operations :: [Operation]
  }

data BinaryOperator
  = Add
  | Mul

data UnaryOperator
  = Neg

data Operation
  = BinaryOperation
      { result :: Identifier,
        binaryOp :: BinaryOperator,
        operands :: (Operand, Operand)
      }
  | UnaryOperation
      { result :: Identifier,
        unaryOp :: UnaryOperator,
        operand :: Operand
      }
  | CallOperation
      { result :: Identifier,
        args :: [Operand]
      }
  | Return
      { val :: Operand
      }

data Method = Method
  { name :: Text,
    params :: [Identifier],
    body :: [Operation]
  }

data SSAForm = SSAForm

data SSAState = SSAState
  { nextBlockLabel :: BlockLabel,
    nextIdentifier :: Int
  }

newtype SSAGen a = SSAGen {runSSAGen :: State SSAState a}
  deriving (Functor, Applicative, Monad, MonadState SSAState)

genBlockLabel :: SSAGen BlockLabel
genBlockLabel = do
  l <- gets nextBlockLabel
  modify (\s -> s {nextBlockLabel = l + 1})
  return l

genIdentifier :: Type -> SSAGen Identifier
genIdentifier t = do
  id <- gets nextIdentifier
  modify (\s -> s {nextIdentifier = id + 1})
  return $ Identifier id t

methodParams :: IR.MethodDecl -> SSAGen [(IR.Name, Identifier)]
methodParams md@(IR.MethodDecl sig@(IR.MethodSig _ _ args) _) = do 
  args <&> (\a@(IR.Argument nm tpe) -> )

generateMethodSSA :: IR.MethodDecl -> SSAGen Method
generateMethodSSA md@(IR.MethodDecl sig block) = do
  _

--generateSSA :: IR.IRRoot -> Map IR.ScopeID SE.SymbolTable -> SSAForm
--generateSSA ir st = _
