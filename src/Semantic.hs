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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Semantic
  ( runSemanticAnalysis,
    generate,
  )
where

import Constants
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import IR
import qualified Parser as P
import Text.Printf (printf)

---------------------------------------
-- Semantic informations and errors
---------------------------------------

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
data SemanticError = SemanticError P.Posn ByteString
  deriving (Show)

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
data SemanticException = SemanticException P.Posn ByteString
  deriving (Show)

data BlockType = RootBlock | IfBlock | ForBlock | WhileBlock | MethodBlock MethodSig
  deriving (Show, Eq)

-- symbol table definitions
data SymbolTable = SymbolTable
  { scopeID :: ScopeID,
    parent :: Maybe SymbolTable,
    importSymbols :: Maybe (Map Name ImportDecl),
    variableSymbols :: Map Name FieldDecl,
    methodSymbols :: Maybe (Map Name MethodDecl),
    blockType :: BlockType
  }

instance Show SymbolTable where
  show (SymbolTable sid p imports variables methods tpe) =
    printf "SymbolTable {scopeID=%d, parent=%s, importSymbols=%s, variableSymbols=%s, methodSymbols=%s, blockType=%s}"
    sid (show $ scopeID <$> p) (show imports) (show variables) (show methods) (show tpe)

data SemanticState = SemanticState
  { nextScopeID :: ScopeID,
    currentScopeID :: ScopeID,
    symbolTables :: Map ScopeID SymbolTable,
    currentPosn :: P.Posn
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
        Left (SemanticException (P.Posn row col) msg) -> Left $ printf "(%d:%d) %s" row col $ B.toString msg
        Right a -> Right (a, errors, state)

initialSemanticState :: SemanticState
initialSemanticState =
  let globalST =
        SymbolTable
          { scopeID = globalScopeID,
            parent = Nothing,
            importSymbols = Just Map.empty,
            variableSymbols = Map.empty,
            methodSymbols = Just Map.empty,
            blockType = RootBlock
          }
   in SemanticState
        { nextScopeID = globalScopeID + 1,
          currentScopeID = globalScopeID,
          symbolTables = Map.fromList [(globalScopeID, globalST)],
          currentPosn = P.Posn {row = 0, col = 0}
        }

-- throw exception or store errors
throwSemanticException :: String -> Semantic a
throwSemanticException msg = do
  posn <- getPosn
  throwError $ SemanticException posn (B.fromString msg)

addSemanticError :: String -> Semantic ()
addSemanticError msg = do
  posn <- getPosn
  tell $ [SemanticError posn (B.fromString msg)]

-- get or update position information
updatePosn :: P.Posn -> Semantic ()
updatePosn posn = do
  st <- get
  put st{currentPosn = posn}

getPosn :: Semantic P.Posn
getPosn = do
  SemanticState {currentPosn = posn} <- get
  return posn

-- find symbol table for blogal scope
getGlobalSymbolTable' :: Semantic (SymbolTable)
getGlobalSymbolTable' = do
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

getMethodSignature :: Semantic (Maybe MethodSig)
getMethodSignature = do
  st <- getSymbolTable
  return $ findMethodSig st
  where
    findMethodSig Nothing = Nothing
    findMethodSig (Just (SymbolTable _ _ _ _ _ RootBlock)) = Nothing
    findMethodSig (Just (SymbolTable _ _ _ _ _ (MethodBlock sig))) = Just sig
    findMethodSig (Just (SymbolTable _ parent _ _ _ _)) = findMethodSig parent

getMethodSignature' :: Semantic MethodSig
getMethodSignature' = do
  sig <- getMethodSignature
  case sig of
    Nothing -> throwSemanticException $ "Cannot find signature for current function!"
    Just s -> return s

enterScope :: BlockType -> Semantic ScopeID
enterScope blockType = do
  state <- get
  parentST <- getSymbolTable
  let nextID = nextScopeID state
      localST =
        SymbolTable
          { scopeID = nextID,
            parent = parentST,
            variableSymbols = Map.empty,
            importSymbols = Nothing,
            methodSymbols = Nothing,
            blockType = blockType
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
Semantic rules to be checked, this will be referenced as Semantic[n]in comments below.
1. Identifier duplication.
2. Identifier should be declared before used.
3. Check for method "main". Also check the parameters and return type.
4. Array length should be greater than 0.
5. Method call has matching type and number of arguments.
6. Method must return something if used in expressions.
7. String literals and array variables may not be used as args to non-import methods.
8. Method declared without a return type shall return nothing.
9. Method return type should match declared type.
10. id used as location should name a variable or parameter.
11. Method should be declared or imported before used.
12. Array location must refer to an array varaible, also the index expression must be of type int.
13. Argument of len operator must be an array.
14. The expression of 'if' and 'when', as well as the second expression of 'for' must have type 'bool'.
15. In a conditional expression (?:):
    The first expression must have type bool.
    The alternatives must have the same type.
16. The operands of the unary negative operator, arithmetic ops and relational ops must have type int.
17. The operands of equal ops must have the same type.
18. The operands of the logical not op and conditional op must have type bool.
19. The location and expression in an assignment must have the same type.
20. The location and expression in an incremental assignment must have type int.
21. All break and continue statment must be within a for or while loop.
22. All int literals must be in the range of -9223372036854775808 ≤ x ≤ 9223372036854775807
(64 bits).
-}

{-
  Helper functions.
-}

-- lookup symbol in local scope.
-- this is useful to find conflicting symbol names.
lookupLocalFieldDecl :: Name -> Semantic (Maybe FieldDecl)
lookupLocalFieldDecl name = do
  table <- getSymbolTable
  return $ table >>= (Map.lookup name . variableSymbols)

lookupVariable' :: Name -> Semantic (Either Argument FieldDecl)
lookupVariable' name = do
  v <- lookupFieldDecl name
  -- Semantic[2]
  case v of
    Just v' -> return $ Right v'
    Nothing -> do
      arg <- lookupArgument name
      case arg of
        Nothing -> throwSemanticException $ printf "Varaible %s not defined" (B.toString name)
        Just a -> return $ Left a

lookupArgument :: Name -> Semantic (Maybe Argument)
lookupArgument name = do
  methodSig <- getMethodSignature
  case methodSig of
    Nothing -> return Nothing
    Just sig -> return $ listToMaybe $ filter (\(Argument nm _) -> nm == name) (args (sig :: MethodSig))

lookupFieldDecl :: Name -> Semantic (Maybe FieldDecl)
lookupFieldDecl name = do
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
  let nm = name ((sig def) :: MethodSig)
  case Map.lookup nm methodTable of
    Just _ -> addSemanticError $ printf "duplicate definition for method %s" $ B.toString nm
    _ -> return ()
  let methodSymbols' = Map.insert nm def methodTable
      newST = localST {methodSymbols = Just methodSymbols'}
  updateSymbolTable newST

-- Semantic[8] and Semantic[9]
checkReturnType :: Maybe (WithType Expr) -> Semantic ()
checkReturnType Nothing = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Nothing -> return ()
    t -> addSemanticError $ printf "Method %s expects return type of %s!" (B.toString method) (show t)
checkReturnType (Just (WithType _ tpe')) = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Nothing -> addSemanticError $ printf "Method %s expects no return value!" (B.toString method)
    t
      | t /= tpe ->
        addSemanticError $
          printf
            "Method %s expects return type of %s, but got %s instead."
            (B.toString method)
            (show tpe)
            (show tpe')
    _ -> return ()

-- | Check if content of lit is a valid int64.
-- lit should be striped of whitespace from both ends and contains only
-- numeric characters or the minus sign '-'.
-- -9223372036854775808 ≤ x ≤ 9223372036854775807
-- checks Semantic[22].
checkInt64Literal :: String -> Semantic Int64
checkInt64Literal lit = do
  if null lit
    then throwSemanticException $ "Cannot parse int literal from an empty token!"
    else return ()
  let isNegative = (head lit) == '-'
  if (isNegative && (drop 1 lit) <= "9223372036854775808")
    || (not isNegative && lit <= "9223372036854775807")
    then return $ (read lit)
    else throwSemanticException $ printf "Int literal %s is out of bound" lit

{-
  Methods to generate ir piece by piece.
-}

generate :: P.Program -> Semantic IRRoot
generate (P.Program imports fields methods) = do
  imports' <- irgenImports imports
  variables' <- irgenFieldDecls fields
  methods' <- irgenMethodDecls methods

  -- check method "main"
  -- Semantic[3]
  globalTable <- getGlobalSymbolTable'
  let main = do
        methodSyms <- methodSymbols globalTable
        Map.lookup mainMethodName methodSyms
  mainDecl <- checkMainExist main
  let mainSig = sig mainDecl
  checkMainRetType $ tpe (mainSig :: MethodSig)
  checkMainArgsType $ args (mainSig :: MethodSig)
  return $ IRRoot imports' variables' methods'
  where
    checkMainExist main = do
      case main of
        Nothing -> throwSemanticException $ printf "Method \"main\" not found!"
        Just decl -> return decl
    checkMainRetType tpe = case tpe of
      Nothing -> return ()
      Just tpe ->
        addSemanticError $
          printf
            "Method \"main\" should have return type of void, got %s instead."
            (show tpe)
    checkMainArgsType args = case args of
      [] -> return ()
      _ -> addSemanticError $ "Method \"main\" should have no argument."

irgenType :: P.Type -> Type
irgenType P.IntType = IntType
irgenType P.BoolType = BoolType

irgenImports :: [P.WithPos P.ImportDecl] -> Semantic [ImportDecl]
irgenImports [] = return []
irgenImports ((P.WithPos (P.ImportDecl id) pos) : rest) = do
  updatePosn pos
  let importSymbol = ImportDecl id
  addImportDef importSymbol
  -- TODO: This kind of recursions potentially lead to stack overflows.
  -- For now it should do the job. Will try to fix in the future.
  rest' <- irgenImports rest
  return $ (importSymbol : rest')

irgenFieldDecls :: [P.WithPos P.FieldDecl] -> Semantic [FieldDecl]
irgenFieldDecls [] = return []
irgenFieldDecls ((P.WithPos decl pos) : rest) = do
  updatePosn pos
  fields <- sequence $ convertFieldDecl decl
  vars <- addVariables fields
  rest' <- irgenFieldDecls rest
  return (vars ++ rest')
  where
    convertFieldDecl (P.FieldDecl tpe elems) =
      flip fmap elems $ \case
        (P.WithPos (P.ScalarField id) pos) -> do
          updatePosn pos
          return $ FieldDecl id (irgenType tpe) Nothing
        (P.WithPos (P.VectorField id size) pos) -> do
          updatePosn pos
          sz <- checkInt64Literal (B.toString size)
          return $ FieldDecl id (irgenType tpe) (Just sz)
    addVariables [] = return []
    addVariables (v : vs) = do
      addVariableDef v
      vs' <- addVariables vs
      return (v : vs')

irgenMethodDecls :: [P.WithPos P.MethodDecl] -> Semantic [MethodDecl]
irgenMethodDecls [] = return []
irgenMethodDecls ((P.WithPos decl pos) : rest) = do
  updatePosn pos
  method <- convertMethodDecl decl
  -- Semantic[8] and Semantic[9]
  -- checkMethod method
  addMethodDef method
  rest' <- irgenMethodDecls rest
  return (method : rest')
  where
    convertMethodDecl (P.MethodDecl id returnType arguments block) = do
      let sig = (MethodSig id (irgenType <$> returnType) args)
      block <- irgenBlock (MethodBlock sig) block
      return $ MethodDecl sig block
      where
        args = map (\(P.WithPos (P.Argument id tpe) _) -> Argument id (irgenType tpe)) arguments

irgenBlock :: BlockType -> P.Block -> Semantic Block
irgenBlock blockType (P.Block fieldDecls statements) = do
  nextID <- enterScope blockType
  fields <- irgenFieldDecls fieldDecls
  stmts <- irgenStatements statements
  let block = Block fields stmts nextID
  exitScope
  return block

irgenLocation :: P.Location -> Semantic (WithType Location)
irgenLocation (P.ScalarLocation id) = do
  -- Semantic[10] (checked in lookupVariable')
  def <- lookupVariable' id
  let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz = either (\_ -> Nothing) (\(FieldDecl _ _ sz') -> sz') def
  -- Semantic[12]
  case sz of
    Nothing -> return $ WithType (Location id Nothing def) tpe
    Just _ -> return $ WithType (Location id Nothing def) (ArrayType tpe)
irgenLocation (P.VectorLocation id expr) = do
  (WithType expr' indexTpe) <- irgenExpr expr
  -- Semantic[10] (checked in lookupVariable')
  def <- lookupVariable' id
  let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz = either (\_ -> Nothing) (\(FieldDecl _ _ sz') -> sz') def
  -- Semantic[12]
  case indexTpe of
    IntType -> return ()
    _ -> throwSemanticException "Index must be of int type!"
  case sz of
    Nothing ->
      throwSemanticException $
        printf "Cannot access index of scalar variable %s." (B.toString id)
    Just _ -> return $ WithType (Location id (Just expr') def) tpe

irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
irgenAssign loc (P.AssignExpr op expr) = do
  loc'@(WithType _ tpe) <- irgenLocation loc
  expr'@(WithType _ tpe') <- irgenExpr expr
  -- Semantic[19]
  if tpe == tpe'
    then addSemanticError $ printf "Assign statement has different types: %s and %s" (show tpe) (show tpe')
    else return ()
  let op' = parseAssignOp op
  -- Semantic[20]
  if (op' == IncAssign || op' == DecAssign) && (tpe /= IntType)
    then addSemanticError $ printf "Inc or dec assign only works with int type!"
    else return ()
  return $ Assignment loc' op' (Just expr')
irgenAssign loc (P.IncrementExpr op) = do
  loc'@(WithType _ tpe) <- irgenLocation loc
  let op' = parseAssignOp op
  -- Semantic[20]
  if tpe /= IntType
    then addSemanticError $ printf "Inc or dec operator only works on int type!"
    else return ()
  return $ Assignment loc' op' Nothing

irgenStatements :: [P.WithPos P.Statement] -> Semantic [Statement]
irgenStatements [] = return []
irgenStatements ((P.WithPos s pos) : xs) = do
  updatePosn pos
  s' <- irgenStmt s
  xs' <- irgenStatements xs
  return (s' : xs')

irgenMethod :: P.MethodCall -> Semantic MethodCall
irgenMethod (P.MethodCall method args') = do
  -- Semantic[2] and Semantic[11]
  decl' <- lookupMethod method
  argsWithType <- sequenceA $ irgenImportArg <$> args'
  case decl' of
    Nothing -> do
      currentMethod <- getMethodSignature
      case currentMethod of
        -- Recursive method calling itself
        (Just (MethodSig name _ formal)) | name == method -> do
                                            checkCallingSemantics formal argsWithType
                                            return $ MethodCall method argsWithType
        _ -> throwSemanticException $ printf "method %s not declared!" (B.toString method)
    Just decl -> case decl of
      Left d -> return $ MethodCall method argsWithType
      Right m -> do
        let formal = args (sig m :: MethodSig)
        -- Semantic[5] and Semantic[7]
        checkCallingSemantics formal argsWithType
        return $ MethodCall method argsWithType
  where
    matchPred ((Argument _ tpe), (WithType _ tpe')) = tpe == tpe'
    argName ((Argument name _), _) = name
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
    checkCallingSemantics formal args = do
      checkArgNum formal args
      checkArgType formal args
      checkForArrayArg args

irgenStmt :: P.Statement -> Semantic Statement
irgenStmt (P.AssignStatement loc expr) = do
  assign <- irgenAssign loc expr
  return $ AssignStmt assign
irgenStmt (P.MethodCallStatement method) = do
  method' <- irgenMethod method
  return $ MethodCallStmt method'
irgenStmt (P.IfStatement expr block) = do
  ifBlock <- irgenBlock IfBlock block
  expr'@(WithType _ tpe) <- irgenExpr expr
  -- Semantic[14]
  case tpe of
    BoolType -> return ()
    _ -> addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe)
  return $ IfStmt expr' ifBlock Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
  ifBlock' <- irgenBlock IfBlock ifBlock
  elseBlock' <- irgenBlock IfBlock elseBlock
  expr'@(WithType _ tpe) <- irgenExpr expr
  -- Semantic[14]
  case tpe of
    BoolType -> return ()
    _ -> addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe)
  return $ IfStmt expr' ifBlock' (Just $ elseBlock')
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
  block' <- irgenBlock ForBlock block
  counterExpr' <- irgenExpr counterExpr
  predExpr'@(WithType _ tpe) <- irgenExpr predExpr
  -- Semantic[14]
  case tpe of
    BoolType -> return ()
    _ -> addSemanticError $ printf "The pred of for statment must have type bool, but got %s instead!" (show tpe)
  assign <- irgenAssign loc expr
  return $ ForStmt counter counterExpr' predExpr' assign block'
irgenStmt (P.WhileStatement expr block) = do
  block' <- irgenBlock WhileBlock block
  expr'@(WithType _ tpe) <- irgenExpr expr
  -- Semantic[14]
  case tpe of
    BoolType -> return ()
    _ -> addSemanticError $ printf "The pred of while statment must have type bool, but got %s instead!" (show tpe)
  return $ WhileStmt expr' block'
irgenStmt (P.ReturnExprStatement expr) = do
  expr' <- irgenExpr expr
  -- Semantic[8] and Semantic[9]
  checkReturnType $ Just expr'
  return $ ReturnStmt $ Just expr'
irgenStmt P.ReturnVoidStatement = do
  -- Semantic[8] and Semantic[9]
  checkReturnType Nothing
  return $ ReturnStmt Nothing
irgenStmt P.BreakStatement = do
  SymbolTable {blockType = btpe} <- getSymbolTable'
  -- Semantic[21]
  if btpe == ForBlock || btpe == WhileBlock
    then return ()
    else addSemanticError $ "Found break statement outside for or while block!"
  return $ BreakStmt
irgenStmt P.ContinueStatement = do
  SymbolTable {blockType = btpe} <- getSymbolTable'
  -- Semantic[21]
  if btpe == ForBlock || btpe == WhileBlock
    then return ()
    else addSemanticError $ "Found continue statement outside for or while block!"
  return $ ContinueStmt

{- generate expressions, also do type inference -}
irgenExpr :: P.WithPos P.Expr -> Semantic (WithType Expr)
irgenExpr (P.WithPos (P.LocationExpr loc) pos)  = do
  updatePosn pos
  (WithType loc' tpe) <- irgenLocation loc
  return $ WithType (LocationExpr loc') tpe
irgenExpr (P.WithPos (P.MethodCallExpr method@(P.MethodCall name _)) pos) = do
  updatePosn pos
  method' <- irgenMethod method
  m <- lookupMethod' name
  case m of
    Left _ ->
      throwSemanticException $
        printf "Cannot call imported method %s in expressions." (B.toString name)
    Right (MethodDecl (MethodSig _ tpe _) _) -> do
      case tpe of
        -- Semantic[6]
        Nothing ->
          throwSemanticException $
            printf "Method %s cannot be used in expressions as it returns nothing!" (B.toString name)
        Just tpe' -> return $ WithType (MethodCallExpr method') tpe'
irgenExpr (P.WithPos (P.IntLiteralExpr i) pos) = do
  updatePosn pos
  literalVal <- checkInt64Literal $ B.toString i
  return $ WithType (IntLiteralExpr literalVal) IntType
irgenExpr (P.WithPos (P.BoolLiteralExpr b) pos) = do
  updatePosn pos
  return $ WithType (BoolLiteralExpr $ read $ B.toString b) BoolType
irgenExpr (P.WithPos (P.CharLiteralExpr c) pos) = do
  updatePosn pos
  return $ WithType (CharLiteralExpr $ read $ B.toString c) IntType
irgenExpr (P.WithPos (P.LenExpr id) pos) = do
  updatePosn pos
  def <- lookupVariable' id
  -- Semantic[13]
  case def of
    Left (Argument nm _) -> throwSemanticException $ printf "len cannot operate on argument %s!" (B.toString nm)
    Right (FieldDecl nm _ sz) -> case sz of
      Nothing -> throwSemanticException $ printf "len cannot operate on scalar variable %s!" (B.toString nm)
      Just _ -> return ()
  return $ WithType (LengthExpr id) IntType
irgenExpr (P.WithPos (P.ArithOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[16]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  case (ltp, rtp) of
    (IntType, IntType) ->
      return $ WithType (ArithOpExpr (parseArithOp op) l' r') IntType
    _ -> throwSemanticException "There can only be integer values in arithmetic expressions."
irgenExpr (P.WithPos (P.RelOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[16]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  case (ltp, rtp) of
    (IntType, IntType) ->
      return $ WithType (RelOpExpr (parseRelOp op) l' r') IntType
    _ -> throwSemanticException "There can only be integer values in relational expressions."
irgenExpr (P.WithPos (P.EqOpExpr op l r) pos)  = do
  updatePosn pos
  -- Semantic[17]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  if (ltp, rtp) == (IntType, IntType) || (ltp, rtp) == (BoolType, BoolType)
    then return $ WithType (EqOpExpr (parseEqOp op) l' r') BoolType
    else throwSemanticException "Can only check equality of expressions with the SAME type!"
irgenExpr (P.WithPos (P.CondOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[18]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  case (ltp, rtp) of
    (BoolType, BoolType) ->
      return $ WithType (CondOpExpr (parseCondOp op) l' r') BoolType
    _ -> throwSemanticException "Conditional ops only accept booleans!"
irgenExpr (P.WithPos (P.NegativeExpr expr) pos) = do
  updatePosn pos
  -- Semantic[16]
  expr'@(WithType _ tpe) <- irgenExpr expr
  case tpe of
    IntType -> return $ WithType (NegOpExpr Neg expr') IntType
    _ -> throwSemanticException "Operator \"-\" only accepts integers!"
irgenExpr (P.WithPos (P.NegateExpr expr) pos) = do
  updatePosn pos
  -- Semantic[18]
  expr'@(WithType _ tpe) <- irgenExpr expr
  case tpe of
    BoolType -> return $ WithType (NotOpExpr Not expr') BoolType
    _ -> throwSemanticException "Operator \"!\" only accepts integers!"
irgenExpr (P.WithPos (P.ChoiceExpr pred l r) pos) = do
  updatePosn pos
  pred'@(WithType _ ptp) <- irgenExpr pred
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  -- Semantic[15]
  case ptp of
    BoolType ->
      if ltp == rtp
        then return $ WithType (ChoiceOpExpr Choice pred' l' r') ltp
        else throwSemanticException "Alternatives of choice op should have same type!"
    _ -> throwSemanticException "Predicate of choice operator must be a boolean!"
irgenExpr (P.WithPos (P.ParenExpr expr) pos) = do
  updatePosn pos
  irgenExpr expr

irgenImportArg :: P.ImportArg -> Semantic (WithType Expr)
irgenImportArg (P.ExprImportArg expr) = irgenExpr expr
irgenImportArg (P.StringImportArg (P.WithPos arg pos)) = do
  updatePosn pos
  return $ WithType (StringLiteralExpr arg) StringType
