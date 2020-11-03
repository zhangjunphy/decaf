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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IR
  ( generate,
    Location (..),
    Assignment (..),
    MethodCall (..),
    IRRoot (..),
    ImportDecl (..),
    FieldDecl (..),
    MethodDecl (..),
    Statement (..),
    Expr (..),
    Block (..),
    Name,
    AssignOp,
    Type,
    SemanticState,
    SemanticException,
    SemanticError,
    runSemanticAnalysis,
  )
where

import Constants
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Parser as P
import Text.Printf (printf)

type Name = ByteString

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
  | ArrayType Type
  deriving (Show, Eq)

parseArithOp :: ByteString -> ArithOp
parseArithOp op = case op of
  "+" -> Plus
  "-" -> Minus

parseRelOp :: ByteString -> RelOp
parseRelOp op = case op of
  "<" -> LessThan
  ">" -> GreaterThan
  "<=" -> LessEqual
  ">=" -> GreaterEqual

parseEqOp :: ByteString -> EqOp
parseEqOp op = case op of
  "==" -> Equal
  "!=" -> NotEqual

parseCondOp :: ByteString -> CondOp
parseCondOp op = case op of
  "||" -> OR
  "&&" -> AND

parseNegOp :: ByteString -> NegOp
parseNegOp op = case op of
  "-" -> Neg

parseNotOp :: ByteString -> NotOp
parseNotOp op = case op of
  "!" -> Not

parseAssignOp :: ByteString -> AssignOp
parseAssignOp s = case s of
  "+=" -> IncAssign
  "-=" -> DecAssign
  "=" -> EqlAssign
  "++" -> PlusPlus
  "--" -> MinusMinus

-- auxiliary data types
data Location = Location
  { name :: Name,
    idx :: Maybe Expr
  }
  deriving (Show)

data Assignment = Assignment
  { location :: Location,
    op :: AssignOp,
    expr :: Maybe Expr
  }
  deriving (Show)

data MethodCall = MethodCall
  { name :: Name,
    args :: [Expr]
  }
  deriving (Show)

-- ir nodes
data IRRoot = IRRoot
  { imports :: [ImportDecl],
    vars :: [FieldDecl],
    methods :: [MethodDecl]
  }
  deriving (Show)

data ImportDecl = ImportDecl {name :: Name}
  deriving (Show)

data FieldDecl = FieldDecl
  { name :: Name,
    tpe :: Type,
    size :: Maybe Int
  }
  deriving (Show)

data MethodDecl = MethodDecl
  { name :: Name,
    tpe :: (Maybe Type),
    args :: [(Name, Type)],
    block :: Block
  }
  deriving (Show)

data Statement
  = AssignStmt {assign :: Assignment}
  | IfStmt {pred :: Expr, ifBlock :: Block, elseBlock :: Maybe Block}
  | ForStmt
      { counter :: Name,
        initCounter :: Expr,
        pred :: Expr,
        update :: Assignment,
        block :: Block
      }
  | WhileStmt {pred :: Expr, block :: Block}
  | ReturnStmt {expr :: (Maybe Expr)}
  | MethodCallStmt {methodCall :: MethodCall}
  | BreakStmt
  | ContinueStmt
  deriving (Show)

data Expr
  = LocationExpr {location :: Location}
  | MethodCallExpr {methodCall :: MethodCall}
  | ExternCallExpr {name :: Name, args :: [Expr]}
  | IntLiteralExpr {intVal :: Int}
  | BoolLiteralExpr {boolVal :: Bool}
  | CharLiteralExpr {charVal :: Char}
  | StringLiteralExpr {strVal :: ByteString}
  | ArithOpExpr {arithOp :: ArithOp, lhs :: Expr, rhs :: Expr}
  | RelOpExpr {relOp :: RelOp, lhs :: Expr, rhs :: Expr}
  | CondOpExpr {condOp :: CondOp, lhs :: Expr, rhs :: Expr}
  | EqOpExpr {eqOp :: EqOp, lhs :: Expr, rhs :: Expr}
  | NegOpExpr {negOp :: NegOp, expr :: Expr}
  | NotOpExpr {notOp :: NotOp, expr :: Expr}
  | ChoiceOpExpr {choiceOp :: ChoiceOp, expr1 :: Expr, expr2 :: Expr, expr3 :: Expr}
  | LengthExpr {name :: Name}
  deriving (Show)

data WithType a = WithType {ele :: a, tpe :: Type}

data Block = Block
  { vars :: [FieldDecl],
    stats :: [Statement],
    blockID :: ScopeID
  }
  deriving (Show)

---------------------------------------
-- Semantic informations and errors
---------------------------------------

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
newtype SemanticError = SemanticError ByteString
  deriving (Show)

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
newtype SemanticException = SemanticException ByteString
  deriving (Show)

-- symbol table definitions
data SymbolTable = SymbolTable
  { scopeID :: ScopeID,
    parent :: Maybe SymbolTable,
    importSymbols :: Maybe (Map Name ImportDecl),
    variableSymbols :: Map Name FieldDecl,
    methodSymbols :: Maybe (Map Name MethodDecl)
  }
  deriving (Show)

data SemanticState = SemanticState
  { nextScopeID :: ScopeID,
    currentScopeID :: ScopeID,
    symbolTables :: Map ScopeID SymbolTable
  }
  deriving (Show)

-- Monad used for semantic analysis
-- Symbol tables are built for every scope, and stored in SemanticState.
-- Semantic errors encountered are recorded by the writer monad (WriterT [SemanticError]).
-- If a serious problem happened such that the analysis has to be aborted, a SemanticException
-- is thrown.
newtype Semantic a = Semantic {runSemantic :: ExceptT SemanticException (WriterT [SemanticError] (State SemanticState)) a}
  deriving (Functor, Applicative, Monad, MonadError SemanticException, MonadWriter [SemanticError], MonadState SemanticState)

runSemanticAnalysis :: Semantic a -> Either String (a, [SemanticError], SemanticState)
runSemanticAnalysis s =
  let ((except, errors), state) = (runState $ runWriterT $ runExceptT $ runSemantic s) initialSemanticState
   in case except of
        Left (SemanticException msg) -> Left $ B.toString msg
        Right a -> Right (a, errors, state)

initialSemanticState :: SemanticState
initialSemanticState =
  let globalST =
        SymbolTable
          { scopeID = globalScopeID,
            parent = Nothing,
            importSymbols = Just Map.empty,
            variableSymbols = Map.empty,
            methodSymbols = Just Map.empty
          }
   in SemanticState
        { nextScopeID = globalScopeID + 1,
          currentScopeID = globalScopeID,
          symbolTables = Map.fromList [(globalScopeID, globalST)]
        }

throwSemanticException :: String -> Semantic a
throwSemanticException msg = throwError $ SemanticException $ B.fromString msg

addSemanticError :: String -> Semantic ()
addSemanticError msg = tell $ [SemanticError (B.fromString msg)]

-- find symbol table for blogal scope
getGlobalSymbolTable :: Semantic (SymbolTable)
getGlobalSymbolTable = do
  state <- get
  case Map.lookup globalScopeID $ symbolTables state of
    Nothing -> throwSemanticException $ printf "No global symbol table found!"
    Just t -> return t

-- find symbol table for current scope
getSymbolTable :: Semantic (Maybe SymbolTable)
getSymbolTable = do
  state <- get
  let id = currentScopeID state
  return $ Map.lookup id $ symbolTables state

getCurrentScopeID :: Semantic Int
getCurrentScopeID = do
  state <- get
  return $ currentScopeID state

-- find symbol table for current scope
-- will throw SemanticException if nothing is found
getSymbolTable' :: Semantic SymbolTable
getSymbolTable' = do
  scopeID <- getCurrentScopeID
  t <- getSymbolTable
  case t of
    Nothing ->
      throwSemanticException $
        printf "No symble table found for current scope %d" $ scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map Name FieldDecl)
getLocalVariables' = do
  localST <- getSymbolTable'
  return $ variableSymbols localST

getLocalImports' :: Semantic (Map Name ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case importSymbols localST of
    Nothing -> throwSemanticException $ printf "No import table for scope %d" $ scopeID localST
    Just t -> return t

getLocalMethods' :: Semantic (Map Name MethodDecl)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methodSymbols localST of
    Nothing -> throwSemanticException $ printf "No method table for scope %d" $ scopeID localST
    Just t -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state {symbolTables = Map.insert (currentScopeID state) t (symbolTables state)}

-- lookup symbol in local scope.
-- this is useful to find conflicting symbol names.
lookupLocalVariable :: Name -> Semantic (Maybe FieldDecl)
lookupLocalVariable name = do
  table <- getSymbolTable
  return $ table >>= (Map.lookup name . variableSymbols)

lookupVariable' :: Name -> Semantic (FieldDecl)
lookupVariable' name = do
  v <- lookupVariable name
  case v of
    Nothing -> throwSemanticException $ printf "Variable %s not defined" (B.toString name)
    Just v' -> return v'

lookupVariable :: Name -> Semantic (Maybe FieldDecl)
lookupVariable name = do
  local <- getSymbolTable
  return $ local >>= lookup name
  where
    lookup name table = case Map.lookup name (variableSymbols table) of
      -- found in local symbol table
      Just s -> Just s
      -- otherwise search in parent table
      Nothing -> case (parent table) of
        Nothing -> Nothing
        Just p -> lookup name p

lookupLocalMethodFromTable :: Name -> SymbolTable -> Maybe (Either ImportDecl MethodDecl)
lookupLocalMethodFromTable name table =
  let method = do
        methodTable <- methodSymbols table
        method' <- Map.lookup name methodTable
        return method'
      import' = do
        importTable <- importSymbols table
        import'' <- Map.lookup name importTable
        return import''
   in case method of
        Just m -> Just $ Right m
        Nothing -> case import' of
          Just im -> Just $ Left im
          Nothing -> Nothing

lookupLocalMethod :: Name -> Semantic (Maybe (Either ImportDecl MethodDecl))
lookupLocalMethod name = do
  table <- getSymbolTable
  return $ table >>= lookupLocalMethodFromTable name

lookupMethod' :: Name -> Semantic (Either ImportDecl MethodDecl)
lookupMethod' name = do
  m <- lookupMethod name
  case m of
    Nothing -> throwSemanticException $ printf "Method %s not found" (B.toString name)
    Just m' -> return m'

lookupMethod :: Name -> Semantic (Maybe (Either ImportDecl MethodDecl))
lookupMethod name = do
  local <- getSymbolTable'
  return $ lookup name local
  where
    lookup name table = case lookupLocalMethodFromTable name table of
      -- found in local symbol table
      Just s -> Just s
      -- otherwise search in parent table
      Nothing -> case (parent table) of
        Nothing -> Nothing
        Just p -> lookup name p

enterScope :: Semantic ScopeID
enterScope = do
  state <- get
  parentST <- getSymbolTable
  let nextID = nextScopeID state
      localST =
        SymbolTable
          { scopeID = nextID,
            parent = parentST,
            variableSymbols = Map.empty,
            importSymbols = Nothing,
            methodSymbols = Nothing
          }
  put $
    state
      { nextScopeID = nextID + 1,
        currentScopeID = nextID,
        symbolTables = Map.insert nextID localST $ symbolTables state
      }
  return nextID

exitScope :: Semantic ()
exitScope = do
  state <- get
  localST <- getSymbolTable
  case localST of
    Nothing ->
      throwSemanticException $
        printf "No symbol table is associated with scope(%d)!" $ currentScopeID state
    Just table ->
      case parent table of
        Nothing ->
          throwSemanticException "Cannot exit root scope!"
        Just p ->
          put $ state {currentScopeID = scopeID p}

----------------------------------------------------------------------
-- Convert the parser tree into an IR tree
-- Generate symbol tables at the same time.
-- Also detects semantic errors.
----------------------------------------------------------------------

{-
Semantic rules to be checked, this will be referenced as Semantic[n]in below.
1. Identifier duplication.
2. Identifier should be declared before used.
3. Check for method "main". Also check the parameters and return type.
4. Array length should be greater than 0.
5. Method call has matching type and number of arguments.
6. Method must return something if used in expressions.
7. String literals and array variables may not be used as args to non-import methods.
8. Method declared without a return type shall return nothing.
-}

addVariableDef :: FieldDecl -> Semantic ()
addVariableDef def = do
  localST <- getSymbolTable'
  -- Semantic[1]
  let nm = name (def :: FieldDecl)
  case Map.lookup (name (def :: FieldDecl)) (variableSymbols localST) of
    Just _ -> addSemanticError $ printf "duplicate definition for variable %s" $ B.toString nm
    _ -> return ()
  let variableSymbols' = Map.insert (name (def :: FieldDecl)) def (variableSymbols localST)
      newST = localST {variableSymbols = variableSymbols'}
  -- Semantic[4]
  case size def of
    Just s -> if s < 0 then addSemanticError (printf "Invalid size of array %s" $ B.toString nm) else tell []
    _ -> return ()
  updateSymbolTable newST

addImportDef :: ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  -- Semantic[1]
  let nm = name (def :: ImportDecl)
  case Map.lookup (name (def :: ImportDecl)) importTable of
    Just _ -> addSemanticError $ printf "duplicate import %s" $ B.toString nm
    _ -> return ()
  let importSymbols' = Map.insert (name (def :: ImportDecl)) def importTable
      newST = localST {importSymbols = Just importSymbols'}
  updateSymbolTable newST

addMethodDef :: MethodDecl -> Semantic ()
addMethodDef def = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  -- Semantic[1]
  let nm = name (def :: MethodDecl)
  case Map.lookup (name (def :: MethodDecl)) methodTable of
    Just _ -> addSemanticError $ printf "duplicate definition for method %s" $ B.toString nm
    _ -> return ()
  let methodSymbols' = Map.insert (name (def :: MethodDecl)) def methodTable
      newST = localST {methodSymbols = Just methodSymbols'}
  updateSymbolTable newST

generate :: P.Program -> Semantic IRRoot
generate (P.Program imports fields methods) = do
  imports' <- irgenImports imports
  variables' <- irgenFieldDecls fields
  methods' <- irgenMethodDecls methods

  -- check method "main"
  -- Semantic[3]
  globalTable <- getGlobalSymbolTable
  let main = do
        methodSyms <- methodSymbols globalTable
        Map.lookup mainMethodName methodSyms
  case main of
    Nothing -> addSemanticError $ printf "Method \"main\" not found!"
    Just methodDecl -> case tpe (methodDecl :: MethodDecl) of
      Nothing -> case args (methodDecl :: MethodDecl) of
        [] -> return ()
        _ -> addSemanticError $ "Method \"main\" should have no argument."
      _ -> addSemanticError $ "Method \"main\" should have return type of void."

  return $ IRRoot imports' variables' methods'

irgenType :: P.Type -> Type
irgenType P.IntType = IntType
irgenType P.BoolType = BoolType

irgenImports :: [P.ImportDecl] -> Semantic [ImportDecl]
irgenImports [] = return []
irgenImports ((P.ImportDecl id) : rest) = do
  let importSymbol = ImportDecl id
  addImportDef importSymbol
  -- TODO: This kind of recursions potentially lead to stack overflows.
  -- For now it should do the job. Will try to fix in the future.
  rest' <- irgenImports rest
  return $ (importSymbol : rest')

irgenFieldDecls :: [P.FieldDecl] -> Semantic [FieldDecl]
irgenFieldDecls [] = return []
irgenFieldDecls (decl : rest) = do
  let fields = convertFieldDecl decl
  vars <- addVariables fields
  rest' <- irgenFieldDecls rest
  return (vars ++ rest')
  where
    convertFieldDecl (P.FieldDecl tpe elems) =
      flip fmap elems $ \case
        (P.ScalarField id) ->
          FieldDecl id (irgenType tpe) Nothing
        (P.VectorField id size) ->
          FieldDecl id (irgenType tpe) (Just sz)
          where
            sz = read $ B.toString size
    addVariables [] = return []
    addVariables (v : vs) = do
      addVariableDef v
      vs' <- addVariables vs
      return (v : vs')

irgenMethodDecls :: [P.MethodDecl] -> Semantic [MethodDecl]
irgenMethodDecls [] = return []
irgenMethodDecls (decl : rest) = do
  method <- convertMethodDecl decl
  addMethodDef method
  rest' <- irgenMethodDecls rest
  return (method : rest')
  where
    convertMethodDecl (P.MethodDecl id returnType arguments block) = do
      block <- irgenBlock block
      return $ MethodDecl id (irgenType <$> returnType) args block
      where
        args = map (\(P.Argument id tpe) -> (id, irgenType tpe)) arguments

irgenBlock :: P.Block -> Semantic Block
irgenBlock (P.Block fieldDecls statements) = do
  nextID <- enterScope
  fields <- irgenFieldDecls fieldDecls
  stmts <- irgenStatements statements
  let block = Block fields stmts nextID
  exitScope
  return block

irgenLocation :: P.Location -> Semantic Location
irgenLocation (P.ScalarLocation id) = do
  -- Semantic[2]
  id' <- lookupVariable id
  case id' of
    Nothing -> throwSemanticException $ printf "variable not decalred %s" (B.toString id)
    _ -> return $ Location id Nothing
irgenLocation (P.VectorLocation id expr) = do
  (WithType expr' _) <- irgenExpr expr
  -- Semantic[2]
  id' <- lookupVariable id
  case id' of
    Nothing -> throwSemanticException $ printf "variable not decalred %s" (B.toString id)
    _ -> return $ Location id (Just expr')

irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
irgenAssign loc (P.AssignExpr op expr) = do
  loc' <- irgenLocation loc
  (WithType expr' _) <- irgenExpr expr
  let op' = parseAssignOp op
  return $ Assignment loc' op' (Just expr')
irgenAssign loc (P.IncrementExpr op) = do
  loc' <- irgenLocation loc
  let op' = parseAssignOp op
  return $ Assignment loc' op' Nothing

irgenStatements :: [P.Statement] -> Semantic [Statement]
irgenStatements [] = return []
irgenStatements (s : xs) = do
  s' <- irgenStmt s
  xs' <- irgenStatements xs
  return (s' : xs')

irgenMethod :: P.MethodCall -> Semantic MethodCall
irgenMethod (P.MethodCall method args') = do
  -- Semantic[2]
  decl' <- lookupMethod method
  argsWithType <- sequenceA $ irgenImportArg <$> args'
  let args'' = (\(WithType e _) -> e) <$> argsWithType
  case decl' of
    Nothing -> throwSemanticException $ printf "method %s not declared!" (B.toString method)
    Just decl -> case decl of
      Left _ -> return $ MethodCall method args''
      Right m -> do
        let formal = args (m :: MethodDecl)
        -- Semantic[5]
        checkArgNum formal argsWithType
        checkArgType formal argsWithType
        -- Semantic[7]
        checkForArrayArg argsWithType
        return $ MethodCall method args''
  where
    matchPred ((_, tpe), (WithType _ tpe')) = tpe == tpe'
    argName ((name, _), _) = name
    checkArgNum formal args =
      if (length formal) == (length args)
        then return ()
        else
          throwSemanticException $
            printf
              "Calling %s with wrong number of args. Required: %d, supplied: %d."
              (B.toString method)
              (length formal)
              (length args)
    checkArgType formal args =
      let mismatch = map argName $ filter (not . matchPred) $ zip formal args
       in if not (null mismatch)
            then
              throwSemanticException $
                printf
                  "Calling %s with wrong type of args: %s"
                  (B.toString method)
                  (show mismatch)
            else return ()
    arrayOrStringTypePred (WithType _ tpe) = case tpe of
      ArrayType _ -> True
      StringType -> True
      _ -> False
    checkForArrayArg args =
      let arrayArgs = map ele $ filter arrayOrStringTypePred args
       in if not $ null arrayArgs
            then
              throwSemanticException $
                printf
                  "Argument of array or string type can not be used for method %s"
                  (B.toString method)
            else return ()

irgenStmt :: P.Statement -> Semantic Statement
irgenStmt (P.AssignStatement loc expr) = do
  assign <- irgenAssign loc expr
  return $ AssignStmt assign
irgenStmt (P.MethodCallStatement method) = do
  method' <- irgenMethod method
  return $ MethodCallStmt method'
irgenStmt (P.IfStatement expr block) = do
  ifBlock <- irgenBlock block
  (WithType expr' _) <- irgenExpr expr
  return $ IfStmt expr' ifBlock Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
  ifBlock' <- irgenBlock ifBlock
  elseBlock' <- irgenBlock elseBlock
  (WithType expr' _) <- irgenExpr expr
  return $ IfStmt expr' ifBlock' (Just $ elseBlock')
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
  block' <- irgenBlock block
  (WithType counterExpr' _) <- irgenExpr counterExpr
  (WithType predExpr' _) <- irgenExpr predExpr
  assign <- irgenAssign loc expr
  return $ ForStmt counter counterExpr' predExpr' assign block'
irgenStmt (P.WhileStatement expr block) = do
  block' <- irgenBlock block
  (WithType expr' _) <- irgenExpr expr
  return $ WhileStmt expr' block'
irgenStmt (P.ReturnExprStatement expr) = do
  (WithType expr' _) <- irgenExpr expr
  return $ ReturnStmt $ Just expr'
irgenStmt P.ReturnVoidStatement = return $ ReturnStmt Nothing
irgenStmt P.BreakStatement = return $ BreakStmt
irgenStmt P.ContinueStatement = return $ ContinueStmt

{- generate expressions, also do type inference -}
irgenExpr :: P.Expr -> Semantic (WithType Expr)
irgenExpr (P.LocationExpr loc) = do
  loc'@(Location name idx) <- irgenLocation loc
  (FieldDecl _ tpe sz) <- lookupVariable' name
  let expr' = WithType (LocationExpr loc') tpe
  case (sz, idx) of
    (Nothing, Nothing) -> return expr'
    (Just _, Just _) -> return expr'
    (Just _, Nothing) -> return $ WithType (LocationExpr loc') $ ArrayType tpe
    (Nothing, Just _) ->
      throwSemanticException $
        printf "Cannot access index of scalar variable %s." (B.toString name)
irgenExpr (P.MethodCallExpr method@(P.MethodCall name _)) = do
  method' <- irgenMethod method
  m <- lookupMethod' name
  case m of
    Left _ ->
      throwSemanticException $
        printf "Cannot call imported method %s in expressions." (B.toString name)
    Right (MethodDecl _ tpe _ _) -> do
      case tpe of
        -- Semantic[6]
        Nothing ->
          throwSemanticException $
            printf "Method %s cannot be used in expressions as it returns nothing!" (B.toString name)
        Just tpe' -> return $ WithType (MethodCallExpr method') tpe'
irgenExpr (P.IntLiteralExpr i) =
  return $ WithType (IntLiteralExpr $ read $ B.toString i) IntType
irgenExpr (P.BoolLiteralExpr b) =
  return $ WithType (BoolLiteralExpr $ read $ B.toString b) BoolType
irgenExpr (P.CharLiteralExpr c) =
  return $ WithType (CharLiteralExpr $ read $ B.toString c) IntType
irgenExpr (P.LenExpr id) = return $ WithType (LengthExpr id) IntType
irgenExpr (P.ArithOpExpr op l r) = do
  (WithType l' ltp) <- irgenExpr l
  (WithType r' rtp) <- irgenExpr r
  case (ltp, rtp) of
    (IntType, IntType) ->
      return $ WithType (ArithOpExpr (parseArithOp op) l' r') IntType
    _ -> throwSemanticException "There can only be integer values in arithmetic expressions."
irgenExpr (P.RelOpExpr op l r) = do
  (WithType l' ltp) <- irgenExpr l
  (WithType r' rtp) <- irgenExpr r
  case (ltp, rtp) of
    (IntType, IntType) ->
      return $ WithType (RelOpExpr (parseRelOp op) l' r') IntType
    _ -> throwSemanticException "There can only be integer values in relational expressions."
irgenExpr (P.EqOpExpr op l r) = do
  (WithType l' ltp) <- irgenExpr l
  (WithType r' rtp) <- irgenExpr r
  if (ltp, rtp) == (IntType, IntType) || (ltp, rtp) == (BoolType, BoolType)
    then return $ WithType (EqOpExpr (parseEqOp op) l' r') BoolType
    else throwSemanticException "Can only check equality of expressions with the SAME type!"
irgenExpr (P.CondOpExpr op l r) = do
  (WithType l' ltp) <- irgenExpr l
  (WithType r' rtp) <- irgenExpr r
  case (ltp, rtp) of
    (BoolType, BoolType) ->
      return $ WithType (CondOpExpr (parseCondOp op) l' r') BoolType
    _ -> throwSemanticException "Conditional ops only accept booleans!"
irgenExpr (P.NegativeExpr expr) = do
  (WithType expr' tpe) <- irgenExpr expr
  case tpe of
    IntType -> return $ WithType (NegOpExpr Neg expr') IntType
    _ -> throwSemanticException "Operator \"-\" only accepts integers!"
irgenExpr (P.NegateExpr expr) = do
  (WithType expr' tpe) <- irgenExpr expr
  case tpe of
    BoolType -> return $ WithType (NotOpExpr Not expr') BoolType
    _ -> throwSemanticException "Operator \"!\" only accepts integers!"
irgenExpr (P.ChoiceExpr pred l r) = do
  (WithType pred' ptp) <- irgenExpr pred
  (WithType l' ltp) <- irgenExpr l
  (WithType r' rtp) <- irgenExpr r
  case ptp of
    BoolType ->
      if ltp == rtp
        then return $ WithType (ChoiceOpExpr Choice pred' l' r') ltp
        else throwSemanticException "Alternatives of choice op should have same type!"
    _ -> throwSemanticException "Predicate of choice operator must be a boolean!"
irgenExpr (P.ParenExpr expr) = irgenExpr expr

irgenImportArg :: P.ImportArg -> Semantic (WithType Expr)
irgenImportArg (P.ExprImportArg expr) = irgenExpr expr
irgenImportArg (P.StringImportArg arg) =
  return $ WithType (StringLiteralExpr arg) StringType
