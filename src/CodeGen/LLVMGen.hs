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
import Data.Char (ord)
import SSA (SSA)
import SSA qualified
import Types (BBID, CompileError (CompileError), Name)
import Util.Graph qualified as G
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Formatting
import Data.List (sort, sortBy, sortOn)

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
convertType AST.CharType = IntType 8
convertType AST.IntType = IntType 64
convertType AST.StringType = PointerType (IntType 8)
convertType (AST.ArrayType tpe len) = ArrayType (convertType tpe) (fromIntegral len)
convertType (AST.Ptr tpe) = PointerType (convertType tpe)

genLLVMIR :: SingleFileCFG -> LLVMGen Module
genLLVMIR (SingleFileCFG declares globals cfgs) = do
  declares' <- genExternalDeclares declares
  globals' <- genGlobals globals
  functions <- mapM genFunction cfgs <&> Map.elems
  return $ Module declares' globals' functions

genExternalDeclares :: [Name] -> LLVMGen [Declare]
genExternalDeclares = mapM $ return . Declare

genGlobals :: [(SSA.Var, AST.Type)] -> LLVMGen [Global]
genGlobals = mapM $ \(var, tpe) -> return $ Global (varName var) (convertType tpe)

genFunction :: CFG -> LLVMGen Function
genFunction cfg@(CFG g _ _ args sig) = do
  let name = sig ^. #name
  let arguments = args <&> genArgument
  bbs <- genCFG cfg
  let retTpe = maybe VoidType convertType (sig ^. #tpe)
  return $ Function name retTpe arguments bbs

genArgument :: SSA.Var -> Argument
genArgument var = Argument (varName var) (convertType $ var ^. #tpe)

genCFG :: CFG -> LLVMGen [BasicBlock]
genCFG cfg@(CFG g _ _ _ _) = do
  let nodes = sortOn fst (G.nodeToList g)
  mapM (uncurry $ genBasicBlock cfg) nodes

genBasicBlock :: CFG -> BBID -> CFG.BasicBlock -> LLVMGen BasicBlock
genBasicBlock (CFG g _ exit _ _) id b = do
  insts <- mapM genInstruction (b ^. #statements) <&> concat
  let outEdges = G.outBound id g
  br <- case outEdges of
    [] -> return []
    [(_, dst, CFG.SeqEdge)] -> return [BrUncon $ Label dst]
    [(_, dst1, CFG.CondEdge (CFG.Pred var)), (_, dst2, CFG.CondEdge (CFG.Complement _))] -> do
      pred <- genImmOrVar var
      return [BrCon pred (Label dst1) (Label dst2)]
    [(_, dst1, CFG.CondEdge (CFG.Complement var)), (_, dst2, CFG.CondEdge (CFG.Pred _))] -> do
      pred <- genImmOrVar var
      return [BrCon pred (Label dst2) (Label dst1)]
    edges -> throwError $ CompileError Nothing (sformat ("Unsupported out bound edge of basicblock: " % shown) edges)
  return $ BasicBlock (Label id) $ insts ++ (br <&> Terminator)

genImmOrVar :: SSA.VarOrImm -> LLVMGen Value
genImmOrVar (SSA.BoolImm True) = return $ IntImm (IntType 1) 1
genImmOrVar (SSA.BoolImm False) = return $ IntImm (IntType 1) 0
genImmOrVar (SSA.IntImm val) = return $ IntImm (IntType 64) val
genImmOrVar (SSA.CharImm val) = return $ IntImm (IntType 4) (fromIntegral $ ord val)
genImmOrVar (SSA.StringImm val) = throwError $ CompileError Nothing "LLVM IR shall not contain any unhandled string literal."
genImmOrVar (SSA.Variable var) = return $ Variable $ genVar var

genVar :: SSA.Var -> Var
genVar var = Var (var ^. #id) (convertType (var ^. #tpe)) (var ^. #loc)

genInstruction :: SSA -> LLVMGen [Instruction]
genInstruction (SSA.Assignment result value) = do
  let res = genVar result
  val <- genImmOrVar value
  return [Assignment res val]
genInstruction (SSA.MethodCall dst name args) = do
  let res = genVar dst
  let args' = args <&> Variable .genVar
  let tpe = convertType (dst ^. #tpe)
  return [Call res tpe name args']
genInstruction (SSA.Return Nothing) = do
  return [Terminator $ Ret Nothing VoidType]
genInstruction (SSA.Return (Just val)) = do
  val' <- genImmOrVar val 
  return [Terminator $ Ret (Just val') (valueType val')]
genInstruction (SSA.Alloca dst tpe sz) = do
  let res = genVar dst
  let tpe' = convertType tpe
  let sz' = fromMaybe 1 sz
  return [MemAccess $ Alloca res tpe' sz']
genInstruction (SSA.Load dst ptr) = do
  let res = genVar dst
  ptr' <- genImmOrVar ptr
  let tpe = convertType (dst ^. #tpe)
  return [MemAccess $ Load res tpe ptr']
genInstruction (SSA.Store ptr src) = do
  src' <- genImmOrVar src
  res <- genImmOrVar ptr
  return [MemAccess $ Store (valueType src') src' res]
genInstruction (SSA.Arith dst op opl opr) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  opl' <- genImmOrVar opl
  opr' <- genImmOrVar opr
  case op of
    AST.Plus -> return [Binary $ Add res tpe opl' opr']
    AST.Minus -> return [Binary $ Sub res tpe opl' opr']
    AST.Multiply -> return [Binary $ Mul res tpe opl' opr']
    AST.Division -> return [Binary $ SDiv res tpe opl' opr']
genInstruction (SSA.Rel dst op opl opr) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  opl' <- genImmOrVar opl
  opr' <- genImmOrVar opr
  case op of 
    AST.LessThan -> return [ICmp res SLT tpe opl' opr']
    AST.GreaterThan -> return [ICmp res SGT tpe opl' opr']
    AST.LessEqual -> return [ICmp res SLE tpe opl' opr']
    AST.GreaterEqual -> return [ICmp res SGE tpe opl' opr']
genInstruction (SSA.Cond dst op opl opr) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  opl' <- genImmOrVar opl
  opr' <- genImmOrVar opr
  case op of 
    AST.And -> return [BitBinary $ And res tpe opl' opr']
    AST.Or -> return [BitBinary $ Or res tpe opl' opr']
genInstruction (SSA.Eq dst op opl opr) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  opl' <- genImmOrVar opl
  opr' <- genImmOrVar opr
  case op of
    AST.Equal -> return [ICmp res EQL tpe opl' opr']
    AST.NotEqual -> return [ICmp res NEQ tpe opl' opr']
genInstruction (SSA.Neg dst op opd) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  zero <- genImmOrVar (SSA.IntImm 0)
  opd' <- genImmOrVar opd
  return [Binary $ Sub res tpe zero opd']
genInstruction (SSA.Not dst op opd) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  let true = IntImm (IntType 1) 1
  opd' <- genImmOrVar opd
  return [Binary $ Sub res tpe true opd']
genInstruction (SSA.Phi dst preds) = do
  let res = genVar dst
  let tpe = convertType (dst ^. #tpe)
  let preds' = preds <&> \(var, label) ->
        let var' = genVar var
        in (var', Label label)
  return [Phi res tpe preds']
genInstruction (SSA.AllocaStr dst content tpe) = do
  let res = genVar dst
  sz <- case tpe of
    AST.ArrayType _ sz' -> return sz'
    _ -> throwError $ CompileError Nothing "String should have array type."
  let charType = convertType AST.CharType
  let alloca = MemAccess $ Alloca res charType sz
  let tpe' = convertType tpe
  let stores = Text.unpack content <&> \c -> (charType, IntImm charType $ fromIntegral $ ord c)
  return [alloca, MemAccess $ StoreVec tpe' stores (Variable res)]
genInstruction inst =
  throwError $ CompileError Nothing (sformat ("Unhandled SSA Instruction:" %+ shown) inst)
