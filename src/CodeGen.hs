-- Semantic -- Decaf IR generator and semantic checker
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
{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import Data.Int (Int64)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified IR
import Semantic

data BasicBlock = BacisBlock
  { name :: Text,
    stmts :: [IR.Statement]
  }

data CFG = CFG
  { basicBlocks :: [BasicBlock]
  }

newtype Codegen a = Codegen {runCodegen :: ExceptT CodegenException (WriterT [Assembly] (State CodegenState)) a}
  deriving (Functor, Applicative, Monad, MonadError CodegenException, MonadWriter [Assembly], MonadState CodegenState)

newtype CodegenException = CodegenException Text

instance Show CodegenException where
  show (CodegenException msg) = show msg

newtype Assembly = Assembly [Text]
  deriving (Show)

data CodegenState = CodegenState
  { textSeg :: [Assembly],
    bssSeg :: [Assembly]
  }
  deriving (Show)

addToBss :: Assembly -> Codegen ()
addToBss asm = do
  st <- get
  put st {bssSeg = bssSeg st ++ [asm]}

addToText :: Assembly -> Codegen ()
addToText asm = do
  st <- get
  put st {textSeg = textSeg st ++ [asm]}

codegenRoot :: IR.IRRoot -> Codegen ()
codegenRoot (IR.IRRoot imports globals methods) = do
  mapM_ codegenGlobalVar globals
  -- mapM_ codegenMethods methods

codegenGlobalVar :: IR.FieldDecl -> Codegen ()
codegenGlobalVar (IR.FieldDecl name tpe sz) = do
  let width = if tpe == IR.BoolType then 1 else 8
      align = if tpe == IR.BoolType then (1 :: Int) else 8
      totalSize = case sz of
        Nothing -> width
        Just sz' -> sz' * width
      asm = [T.pack $ printf ".comm %s, %d, %d" name totalSize align]
  addToText $ Assembly asm

-- codegenMethods :: IR.MethodDecl -> Codegen ()
-- codegenMethods  = _
