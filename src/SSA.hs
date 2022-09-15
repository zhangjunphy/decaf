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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SSA where

import Constants
import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Char (ord)
import Data.Functor
import Data.Int (Int64, Int8)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Formatting
import qualified IR
import qualified Scanner as IR
import qualified Semantic as SE

type BlockLabel = Int

data Type
  = Int64Type
  | Int8Type
  | PtrType Type
  | ArrayType Type Int64
  | VoidType
  deriving (Show)

isIntegerType :: Type -> Bool
isIntegerType Int64Type = True
isIntegerType Int8Type = True
isIntegerType _ = False

convertIRType :: IR.Type -> Type
convertIRType IR.IntType = Int64Type
convertIRType IR.BoolType = Int8Type
convertIRType IR.StringType = PtrType Int8Type
convertIRType (IR.ArrayType t sz) = ArrayType (convertIRType t) sz

typeSize :: Type -> Int64
typeSize Int64Type = 8
typeSize Int8Type = 1
typeSize (PtrType _) = 8
typeSize (ArrayType tpe sz) = sz * typeSize tpe
typeSize VoidType = 0

data Imm
  = Int64Imm Int64
  | Int8Imm Int8
  deriving (Show)

data Identifier = Identifier
  { idx :: Int64,
    tpe :: Type
  }
  deriving (Show)

data Operand
  = Var Identifier
  | ImmVal Imm
  deriving (Show)

typeOfOpd :: Operand -> Type
typeOfOpd (ImmVal (Int64Imm _)) = Int64Type
typeOfOpd (ImmVal (Int8Imm _)) = Int8Type
typeOfOpd (Var (Identifier _ tpe)) = tpe

data Block = Block
  { label :: BlockLabel,
    arguments :: [Identifier],
    operations :: [Operation]
  }
  deriving (Show)

data BinaryOperator
  = Add
  | Minus
  | Mul
  | Div
  | Mod
  | LT'
  | LE'
  | GT'
  | GE'
  | EQ'
  | NE'
  | And
  | Or
  deriving (Show)

convertArithOp :: IR.ArithOp -> BinaryOperator
convertArithOp IR.Plus = Add
convertArithOp IR.Minus = Minus
convertArithOp IR.Multiply = Mul
convertArithOp IR.Division = Div
convertArithOp IR.Modulo = Mod

convertRelOp :: IR.RelOp -> BinaryOperator
convertRelOp IR.LessThan = LT'
convertRelOp IR.LessEqual = LE'
convertRelOp IR.GreaterThan = GT'
convertRelOp IR.GreaterEqual = GE'

convertEqOp :: IR.EqOp -> BinaryOperator
convertEqOp IR.Equal = EQ'
convertEqOp IR.NotEqual = NE'

convertCondOp :: IR.CondOp -> BinaryOperator
convertCondOp IR.AND = And
convertCondOp IR.OR = Or

data UnaryOperator
  = Not
  deriving (Show)

type MethodID = Text

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
      { method :: MethodID,
        ret :: Maybe Identifier,
        args :: [Operand]
      }
  | Return
      { val :: Operand
      }
  | Alloc
      { ptr :: Identifier,
        tpe :: Type,
        size :: Int64
      }
  | Store
      { ptr :: Identifier,
        val :: Operand
      }
  | Load
      { var :: Identifier,
        ptr :: Identifier
      }
  deriving (Show)

data Method = Method
  { name :: Text,
    retType :: Type,
    params :: [Identifier],
    body :: [Operation]
  }
  deriving (Show)

data SSAState = SSAState
  { nextBlockLabel :: BlockLabel,
    nextIdentifier :: Int64,
    symbolToId :: Map IR.ScopeID (Map IR.Name Identifier),
    scopeID :: IR.ScopeID
  }

data SSAInfo = SSAInfo
  { symbolTables :: Map SE.ScopeID SE.SymbolTable
  }

data SSAException = SSAException Text

newtype SSAGen a = SSAGen {runSSAGen :: (ExceptT SSAException (WriterT [Operation] (ReaderT SSAInfo (State SSAState)))) a}
  deriving (Functor, Applicative, Monad, MonadError SSAException, MonadWriter [Operation], MonadReader SSAInfo, MonadState SSAState)

throw :: Text -> SSAGen a
throw msg = do
  throwError $ SSAException msg

throwIf :: Bool -> Text -> SSAGen ()
throwIf cond msg = when cond $ throw msg

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

lookupIdentifier' :: IR.ScopeID -> IR.Name -> SSAGen Identifier
lookupIdentifier' scope nm = do
  s2id <- gets symbolToId
  case Map.lookup scope s2id of
    Nothing -> throw $ sformat ("Cannot find scope " % int) scope
    Just m -> case Map.lookup nm m of
      Nothing -> throw $ sformat ("Cannot find id for var " % stext) nm
      Just id -> return id

getSymbolTable' :: SSAGen SE.SymbolTable
getSymbolTable' = do
  scope <- gets scopeID
  sts <- asks symbolTables
  let st = Map.lookup scope sts
  case st of
    Nothing -> throw $ sformat ("Cannot find symbole table for scope " % int) scope
    Just st' -> return st'

lookupVariable' :: IR.Name -> SSAGen (IR.ScopeID, Either IR.Argument IR.FieldDecl)
lookupVariable' nm = do
  st <- getSymbolTable'
  case Just st >>= lookup nm of
    Nothing -> throw $ sformat ("Cannot find variable " % stext) nm
    Just v -> return v
  where
    checkST name st' = SE.lookupLocalVariableFromST name st' <&> \v -> (SE.scopeID st', v)
    lookup name st' = checkST name st' <|> (SE.parent st' >>= lookup name)

lookupMethod' :: IR.Name -> SSAGen (IR.ScopeID, Either IR.ImportDecl IR.MethodDecl)
lookupMethod' nm = do
  st <- getSymbolTable'
  case Just st >>= lookup nm of
    Nothing -> throw $ sformat ("Cannot find variable " % stext) nm
    Just v -> return v
  where
    checkST name st' = SE.lookupLocalMethodFromST name st' <&> \v -> (SE.scopeID st', v)
    lookup name st' = checkST name st' <|> (SE.parent st' >>= lookup name)

readSymbol :: IR.Name -> SSAGen Identifier
readSymbol nm = do
  (scope, _) <- lookupVariable' nm
  id <- lookupIdentifier' scope nm
  if isGlobalScope scope
    then generatePtrLoad id
    else return id

assocSymbolAndIdentifier :: IR.Name -> Identifier -> SSAGen ()
assocSymbolAndIdentifier nm id = do
  scope <- gets scopeID
  s2id <- gets symbolToId
  let s2id' = case Map.lookup scope s2id of
        Just _ -> s2id
        Nothing -> Map.insert scope Map.empty s2id
  let s2id'' = Map.update (Just . Map.insert nm id) scope s2id'
  s <- get
  put s {symbolToId = s2id''}

addOperation :: Operation -> SSAGen ()
addOperation op = tell [op]

generateBlock :: [(IR.Name, Identifier)] -> IR.Block -> SSAGen ()
generateBlock envVars block@(IR.Block fileds stmts scope) = do
  parentScope <- gets scopeID
  modify' (\s -> s {scopeID = scope})
  forM_ envVars (uncurry assocSymbolAndIdentifier)
  mapM_ generateStmt stmts
  modify' (\s -> s {scopeID = parentScope})

isGlobalScope :: IR.ScopeID -> Bool
isGlobalScope scope = scope == globalScopeID

methodID :: IR.ScopeID -> Text -> MethodID
methodID = sformat (int % "!" % stext)

alloc :: Type -> Int64 -> SSAGen Identifier
alloc tpe size = do
  id <- genIdentifier $ PtrType tpe
  addOperation $ Alloc id tpe size
  return id

load :: Identifier -> SSAGen Identifier
load ptr@(Identifier _ (PtrType tpe)) = do
  id <- genIdentifier tpe
  addOperation $ Load id ptr
  return id
load (Identifier ptrID _) = throw $ sformat ("Cannot load from non-pointer type, id: " % int) ptrID

store :: Identifier -> Operand -> SSAGen ()
store ptr val = do
  addOperation $ Store ptr val

int64Imm :: Int64 -> SSAGen Identifier
int64Imm val = do
  newID <- genIdentifier Int64Type
  addOperation $ BinaryOperation newID Add (ImmVal $ Int64Imm val, ImmVal $ Int64Imm 0)
  return newID

boolImm :: Bool -> SSAGen Identifier
boolImm val = do
  id <- genIdentifier Int8Type
  int8 <- if val then 1 else 0
  addOperation $ BinaryOperation id Add (ImmVal $ Int8Imm int8, ImmVal $ Int8Imm 0)
  return id

charImm :: Char -> SSAGen Identifier
charImm val = do
  id <- genIdentifier Int8Type
  addOperation $ BinaryOperation id Add (ImmVal $ Int8Imm $ fromIntegral val, ImmVal $ Int8Imm 0)
  return id

stringLit :: Text -> SSAGen Identifier
stringLit s = do
  id' <- alloc Int8Type $ fromIntegral length s
  let chars = Text.foldl' (\l c -> l ++ [c]) [] s
  mapM_
    ( \(idx, char) -> do
        ptr <- ptrAdd id' (ImmVal $ Int64Imm $ fromIntegral idx)
        store ptr $ ImmVal $ Int8Imm $ fromIntegral char
    )
    $ zip [1 .. length chars] chars
  return id'

checkBinaryOpType :: Operand -> Operand -> SSAGen Type
checkBinaryOpType l r = do
  let lType = typeOfOpd l
      rType = typeOfOpd r
  throwIf (lType /= rType) "Operands of binary operation must have identical type!"
  throwIf (not (isIntegerType lType)) "Operands of binary operation must be intergers!"
  return lType

binOp :: BinaryOperator -> Operand -> Operand -> SSAGen Identifier
binOp op l r = do
  rType <- checkBinaryOpType l r
  id <- genIdentifier rType
  addOperation $ BinaryOperation id op (l, r)
  return id

unOp :: UnaryOperator -> Operand -> SSAGen Identifier
unOp op@Not v = do
  let tpe = typeOfOpd v
  throwIf (not (isIntegerType tpe)) "Operands of binary operation must be intergers!"
  id <- genIdentifier tpe
  addOperation $ UnaryOperation id op v
  return id

ptrAdd :: Identifier -> Operand -> SSAGen Identifier
ptrAdd ptr@(Identifier _ (PtrType t)) int = do
  id <- genIdentifier t
  addOperation $ BinaryOperation id Add (Var ptr, int)
  return id
ptrAdd (Identifier id _) _ = throw $ sformat ("Trying to add non-pointer type, id: " % int) id

mul :: Operand -> Operand -> SSAGen Identifier
mul l r = do
  rType <- checkBinaryOpType l r
  id <- genIdentifier rType
  addOperation $ BinaryOperation id Mul (l, r)
  return id

call :: MethodID -> [Operand] -> Maybe Type -> SSAGen (Maybe Identifier)
call method args tpe = do
  id <- mapM genIdentifier tpe
  addOperation $ CallOperation method id args
  return id

generatePtrLoad :: Identifier -> SSAGen Identifier
generatePtrLoad ptr@(Identifier i (PtrType t)) = do
  newID <- genIdentifier t
  addOperation $ Load newID ptr
  return newID
generatePtrLoad id = throw $ sformat ("Loading non pointer type " % string) (show id)

arrayLocToPtr :: IR.Location -> SSAGen Identifier
arrayLocToPtr (IR.Location nm (Just sz) def) = do 
  ptr <- readSymbol nm
  (_, def) <- lookupVariable' nm
  let tp = either (\(IR.Argument _ tpe) -> tpe) (\(IR.FieldDecl _ tpe) -> tpe) def
  (eleSize, eleType) <- case convertIRType tp of
    ArrayType t _ -> return (typeSize t, t)
    _ -> throw $ sformat ("Symbol " % stext % "is not of a array type!") nm
  idx <- generateExpr sz
  eleSizeID <- int64Imm eleSize
  offset <- mul (Var eleSizeID) (Var idx)
  ptrAdd ptr (Var offset)
arrayLocToPtr _ = throw "Indexing non array type!"

readLoc :: IR.Location -> SSAGen Identifier
readLoc (IR.Location nm Nothing def) = readSymbol nm
readLoc loc@(IR.Location _ (Just _) _) = arrayLocToPtr loc >>= load

writeLoc :: IR.Location -> Identifier -> SSAGen ()
writeLoc (IR.Location nm Nothing def) v@(Identifier _ tpe) = do
  (scope, _) <- lookupVariable' nm
  id <- lookupIdentifier' scope nm
  let defType = convertIRType $ IR.typeOfDef def
  throwIf (tpe /= defType) $ sformat ("Assign incompatible value to variable " % stext) nm
  if isGlobalScope scope
    then store id (Var v)
    else assocSymbolAndIdentifier nm v
writeLoc loc@(IR.Location nm (Just _) def) v = do 
  ptr <- arrayLocToPtr loc
  store ptr (Var v)

generateExpr :: IR.Expr -> SSAGen Identifier
generateExpr (IR.LocationExpr loc) = readLoc loc
generateExpr (IR.MethodCallExpr (IR.MethodCall nm args)) = do
  (methodScope, def) <- lookupMethod' nm
  retType <- do
    case def of
      Left _ -> throw $ sformat ("Cannot use imported method " % stext % " in expression.") nm
      Right (IR.MethodDecl (IR.MethodSig _ (Just tpe) _) _) -> return $ convertIRType tpe
      _ -> throw $ sformat ("Method " % stext % " with no return val cannot be used in expression.") nm
  ids' <- mapM (\(IR.WithType e _) -> generateExpr e) args
  args' <- mapM (return . Var) ids'
  (Just ret) <- call (methodID methodScope nm) args' (Just retType)
  return ret
generateExpr (IR.ExternCallExpr _ _) = throw "External function call not supported yet!"
generateExpr (IR.IntLiteralExpr lit) = int64Imm lit
generateExpr (IR.BoolLiteralExpr lit) = boolImm lit
generateExpr (IR.CharLiteralExpr lit) = charImm lit
generateExpr (IR.StringLiteralExpr lit) = stringLit lit
generateExpr (IR.ArithOpExpr op (IR.WithType l _) (IR.WithType r _)) = do
  l' <- generateExpr l
  r' <- generateExpr r
  binOp (convertArithOp op) (Var l') (Var r')
generateExpr (IR.RelOpExpr op (IR.WithType l _) (IR.WithType r _)) = do
  l' <- generateExpr l
  r' <- generateExpr r
  binOp (convertRelOp op) (Var l') (Var r')
generateExpr (IR.CondOpExpr op (IR.WithType l _) (IR.WithType r _)) = do
  l' <- generateExpr l
  r' <- generateExpr r
  binOp (convertCondOp op) (Var l') (Var r')
generateExpr (IR.EqOpExpr op (IR.WithType l _) (IR.WithType r _)) = do
  l' <- generateExpr l
  r' <- generateExpr r
  binOp (convertEqOp op) (Var l') (Var r')
generateExpr (IR.NegOpExpr op (IR.WithType v _)) = do
  v' <- generateExpr v
  binOp Minus (ImmVal $ Int64Imm 0) (Var v')
generateExpr (IR.NotOpExpr op (IR.WithType v _)) = do
  v' <- generateExpr v
  unOp Not (Var v')
generateExpr (IR.ChoiceOpExpr op (IR.WithType cond _) (IR.WithType t _) (IR.WithType f _)) = do
  -- convert to if statement ?
  _
generateExpr (IR.LengthExpr nm) = do
  (scope, def) <- lookupVariable' nm
  sz <- case def of
    Left (IR.Argument _ (IR.ArrayType _ sz)) -> return sz
    Right (IR.FieldDecl _ (IR.ArrayType _ sz)) -> return sz
    _nonArray -> throw $ sformat ("Trying to get length of non array type " % stext) nm
  int64Imm sz

generateStmt :: IR.Statement -> SSAGen ()
generateStmt (IR.AssignStmt (IR.Assignment (IR.WithType loc@(IR.Location nm _ _) _) IR.EqlAssign (Just (IR.WithType expr _)))) = do 
  v' <- generateExpr expr
  writeLoc loc v'
generateStmt (IR.AssignStmt (IR.Assignment (IR.WithType loc@(IR.Location nm _ _) _) IR.PlusPlus Nothing)) = do 
  v <- readSymbol nm
  v' <- binOp Add (Var v) (ImmVal $ Int64Imm 1)
  writeLoc loc v'
generateStmt (IR.AssignStmt (IR.Assignment (IR.WithType loc@(IR.Location nm _ _) _) IR.MinusMinus Nothing)) = do 
  v <- readSymbol nm
  v' <- binOp Minus (Var v) (ImmVal $ Int64Imm 1)
  writeLoc loc v'
generateStmt (IR.AssignStmt (IR.Assignment (IR.WithType loc@(IR.Location nm _ _) _) IR.IncAssign (Just (IR.WithType expr _)))) = do 
  v <- readSymbol nm
  inc <- generateExpr expr
  v' <- binOp Add (Var v) (Var inc)
  writeLoc loc v'
generateStmt (IR.AssignStmt (IR.Assignment (IR.WithType loc@(IR.Location nm _ _) _) IR.DecAssign (Just (IR.WithType expr _)))) = do 
  v <- readSymbol nm
  inc <- generateExpr expr
  v' <- binOp Minus (Var v) (Var inc)
  writeLoc loc v'
generateStmt (IR.AssignStmt _) = throw "Invalid assign statement!"
generateStmt (IR.IfStmt (IR.WithType pred _) thenBlock elseBlock) = do
  _

methodParams :: IR.MethodSig -> SSAGen [(IR.Name, Identifier)]
methodParams sig@(IR.MethodSig _ _ args) =
  foldM convertArg [] args
  where
    convertArg = \l (IR.Argument nm tpe) -> do
      let tpe' = convertIRType tpe
      id <- genIdentifier tpe'
      return $ l ++ [(nm, id)]

generateMethod :: IR.MethodDecl -> SSAGen Method
generateMethod md@(IR.MethodDecl sig@(IR.MethodSig nm tpe args) block) = do
  let retType = maybe VoidType convertIRType tpe
  params <- methodParams sig
  body <- generateBlock params block
  return $ Method nm retType (snd <$> params) body

--generateSSA :: IR.IRRoot -> Map IR.ScopeID SE.SymbolTable -> SSAForm
--generateSSA ir st = _
