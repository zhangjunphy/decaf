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

-- This should be a straightforward translation from CFG into LLVM IR

module CodeGen.LLVMGen where

import AST qualified
import CFG (CFG (..), SingleFileCFG (..))
import CFG qualified
import CodeGen.LLVMIR
import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.State
import Data.Functor ((<&>))
import Data.Generics.Labels
import Data.Text (Text)
import Data.Text qualified as Text
import SSA (SSA)
import SSA qualified
import Types (BBID, CompileError (CompileError))
import Util.Graph qualified as G

data LLVMGenState = LLVMGenState

newtype LLVMGen a = LLVMGen
  { runLLVMGen ::
      ExceptT
        CompileError
        (State LLVMGenState)
        a
  }
  deriving
    (Functor, Applicative, Monad, MonadError CompileError, MonadState LLVMGenState)

varName :: SSA.Var -> Text
varName var = Text.pack (show var)

convertType :: AST.Type -> Type
convertType AST.Void = VoidType
convertType AST.BoolType = IntType 1
convertType AST.CharType = IntType 4
convertType AST.IntType = IntType 64
convertType AST.StringType = PointerType (IntType 4)
convertType (AST.ArrayType tpe len) = ArrayType (convertType tpe) (fromIntegral len)
convertType (AST.Ptr tpe) = PointerType (convertType tpe)

genLLVMIR :: SingleFileCFG -> LLVMGen Module
genLLVMIR (SingleFileCFG global cfgs) = do
  globals <- genGlobalBB global
  undefined

genGlobalBB :: CFG.BasicBlock -> LLVMGen [Global]
genGlobalBB (CFG.BasicBlock _ _ insts) = forM insts genGlobalDecl

genGlobalDecl :: SSA -> LLVMGen Global
genGlobalDecl (SSA.InitGlobal dst tpe) = return $ Global (varName dst) (convertType tpe)
genGlobalDecl _ = throwError $ CompileError Nothing "Global basic block contains instruction other than alloc."

genFunction :: CFG -> LLVMGen Function
genFunction (CFG g _ _ args sig) = do
  let name = sig ^. #name
  let arguments = args <&> genArgument
  let bbs = genBasicBlocks g
  return $ Function name arguments bbs

genArgument :: SSA.Var -> Argument
genArgument var = Argument (varName var) (convertType $ var ^. #tpe)

genBasicBlocks :: G.Graph BBID CFG.BasicBlock CFG.CFGEdge -> [BasicBlock]
genBasicBlocks g = undefined
