-- Semantic -- Decaf Semantic Checker
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

module Semantic ( check
                ) where

import Parser
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map

----------------------------------------------------------------------
-- Block Name
----------------------------------------------------------------------

type Name = String
type Names = Map.Map String Int
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
      Nothing -> (nm, Map.insert nm 1 ns)
      Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

----------------------------------------------------------------------
-- Descriptor and Symbol Table
----------------------------------------------------------------------

data FieldDescriptor
    = FieldDescriptor {
        fieldId   :: String
      , fieldIdx  :: Int
      , fieldTpe  :: Type
      , size      :: Maybe Int  -- Scalar field does not have a size.
      } deriving Show

data MethodDescriptor
    = MethodDescriptor {
        methodId   :: String
      , methodIdx  :: Int
      , returnType :: Maybe Type
      , statements :: [Statement]
      } deriving Show

type FieldSymTable = [(String, FieldDescriptor)]

type MethodSymTable = [(String, MethodDescriptor)]

----------------------------------------------------------------------
-- Codegen State
----------------------------------------------------------------------

data BlockState
    = BlockState {
        idx :: Int
      , name :: Name
      , fieldSymTable :: FieldSymTable
      , parent :: Maybe BlockState
      } deriving Show

newBlock :: Int -> Name -> Maybe BlockState -> BlockState
newBlock i name p = BlockState i name [] p

data CodegenState
    = CodegenState {
        currentBlock :: Name
      , names :: Names
      , blocks :: Map.Map Name BlockState
      , imports :: [String]
      , methodSymTable  :: MethodSymTable
      , fieldCount :: Int
      } deriving Show

entryBlockName :: String
entryBlockName = "entry"
entryBlock :: BlockState
entryBlock = newBlock 0 entryBlockName Nothing

newCodegen :: CodegenState
newCodegen =
    CodegenState entryBlockName
                 Map.empty
                 (Map.fromList [(entryBlockName, entryBlock)])
                 [] [] 0

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

----------------------------------------------------------------------
-- Block Operations
----------------------------------------------------------------------

enterBlock :: Name -> Codegen Name
enterBlock bname = do
  c <- current
  bls <- gets blocks
  nms <- gets names
  let (qname, supply) = uniqueName bname nms
      new = newBlock (Map.size bls) qname (Just c)
  modify $ \s -> s { blocks = Map.insert qname new bls
                   , names = supply}
  return qname

leaveBlock :: Codegen Name
leaveBlock = do
  c <- current
  let pname = name c
  modify $ \s -> s { currentBlock = pname }
  return pname

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "Block not found: " ++ show c

----------------------------------------------------------------------
-- Symbol Table operations
----------------------------------------------------------------------

addField :: String -> FieldDescriptor -> FieldSymTable
         -> FieldSymTable
addField var x t = (var, x):t

lookupField :: String -> BlockState -> Maybe FieldDescriptor
lookupField var (BlockState _ _ t Nothing) = lookup var t
lookupField var (BlockState _ _ t (Just p)) =
    case lookup var t of
      Just f -> Just f
      Nothing -> lookupField var p

fieldExist :: String -> BlockState -> Bool
fieldExist var (BlockState _ _ t _) =
    isJust $ find (\s -> (fst s) == var) t

addMethod :: String -> MethodDescriptor -> MethodSymTable
          -> MethodSymTable
addMethod var x t = (var, x):t

declare :: FieldDecl -> Codegen ()
declare (FieldDecl tpe es)= do
  c@(BlockState _ _ ft p) <- current
  cnt <- gets fieldCount
  let (cnt', descriptors) =
          foldl (\(c, lst) elem -> (c+1, (newField c elem):lst))
                    (cnt, []) es
  let symTable' = foldl (\tb desc -> (Semantic.fieldId desc, desc):tb)
                  (fieldSymTable c) descriptors
  let c' = c { fieldSymTable = symTable' }
  modifyBlock c'
  modify' $ \s -> s { fieldCount = cnt' }
  where
    newField idx e
        = case e of
            (ScalarField id) -> FieldDescriptor id idx tpe Nothing
            (VectorField id sz) -> FieldDescriptor id idx tpe
                                   (Just (read sz))

defun :: MethodDecl -> Codegen ()
defun (MethodDecl id tpe args block) = do
  mt <- gets methodSymTable
  let idx = length mt
  let desc = MethodDescriptor id idx tpe (blockStatements block)
  modify $ \s -> s { methodSymTable = (id, desc):mt }

getvar :: String -> Codegen (Maybe FieldDescriptor)
getvar var = do
  c <- current
  return $ lookupField var c

----------------------------------------------------------------------
-- Walk through AST, do semantic checking, and generate an ir
----------------------------------------------------------------------

check :: Int
check = 0

generate :: Parser.Program -> AbstractSyntaxTree
generate = \_ -> AbstractSyntaxTree

newtype IR a = IR (State Parser.Program a)
    deriving (Functor, Applicative, Monad, MonadState Parser.Program)

data AbstractSyntaxTree = AbstractSyntaxTree
                          deriving (Show)

data AstNode = ProgramNode {}
