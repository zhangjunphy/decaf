-- Semantic -- Decaf semantic checker
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
{-# LANGUAGE OverloadedStrings #-}

module Semantic
  ( runSemanticAnalysis,
    SymbolTable (..),
    SemanticState (..),
    ScopeID,
  )
where

import Constants
import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import IR
import qualified Parser as P
import Formatting

---------------------------------------
-- Semantic informations and errors
---------------------------------------

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
data SemanticError = SemanticError P.Posn Text

instance Show SemanticError where
  show (SemanticError (P.Posn row col) msg) = formatToString ("["%int%":"%int%"] "%stext) row col msg

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
data SemanticException = SemanticException P.Posn Text

instance Show SemanticException where
  show (SemanticException (P.Posn row col) msg) = formatToString ("["%int%":"%int%"] "%stext) row col msg

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
    formatToString ("SymbolTable {scopeID="%int%", parent="%shown%", imports="%shown%", variables="%shown%", methods="%shown%", tpe="%shown)
      sid
      (scopeID <$> p)
      (show imports)
      (show variables)
      (show methods)
      (show tpe)

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

runSemanticAnalysis :: P.Program -> Either String (IRRoot, [SemanticError], SemanticState)
runSemanticAnalysis p =
  let ir = irgenRoot p
      ((except, errors), state) = (runState $ runWriterT $ runExceptT $ runSemantic ir) initialSemanticState
   in case except of
        Left (SemanticException (P.Posn row col) msg) -> Left $ formatToString ("["%int%":"%int%"] "%stext) row col msg
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
throwSemanticException :: Text -> Semantic a
throwSemanticException msg = do
  posn <- getPosn
  throwError $ SemanticException posn msg

addSemanticError :: Text -> Semantic ()
addSemanticError msg = do
  posn <- getPosn
  tell [SemanticError posn msg]

-- get or update position information
updatePosn :: P.Posn -> Semantic ()
updatePosn posn = do
  st <- get
  put st {currentPosn = posn}

getPosn :: Semantic P.Posn
getPosn = do
  SemanticState {currentPosn = posn} <- get
  return posn

-- find symbol table for blogal scope
getGlobalSymbolTable' :: Semantic SymbolTable
getGlobalSymbolTable' = do
  state <- get
  case Map.lookup globalScopeID $ symbolTables state of
    Nothing -> throwSemanticException "No global symbol table found!"
    Just t -> return t

-- find symbol table for current scope
getSymbolTable :: Semantic (Maybe SymbolTable)
getSymbolTable = do
  state <- get
  let id = currentScopeID state
  return $ Map.lookup id $ symbolTables state

getCurrentScopeID :: Semantic Int
getCurrentScopeID = gets currentScopeID

-- find symbol table for current scope
-- will throw SemanticException if nothing is found
getSymbolTable' :: Semantic SymbolTable
getSymbolTable' = do
  scopeID <- getCurrentScopeID
  t <- getSymbolTable
  case t of
    Nothing ->
      throwSemanticException $ sformat ("No symble table found for current scope "%int) scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map Name FieldDecl)
getLocalVariables' = do
  variableSymbols <$> getSymbolTable'

getLocalImports' :: Semantic (Map Name ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case importSymbols localST of
    Nothing -> throwSemanticException $ sformat ("No import table for scope "%int) $ scopeID localST
    Just t -> return t

getLocalMethods' :: Semantic (Map Name MethodDecl)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methodSymbols localST of
    Nothing -> throwSemanticException $ sformat ("No method table for scope "%int) $ scopeID localST
    Just t -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state {symbolTables = Map.insert (currentScopeID state) t (symbolTables state)}

getMethodSignatureFromST :: SymbolTable -> Maybe MethodSig
getMethodSignatureFromST (SymbolTable _ _ _ _ _ RootBlock) = Nothing
getMethodSignatureFromST (SymbolTable _ _ _ _ _ (MethodBlock sig)) = Just sig
getMethodSignatureFromST (SymbolTable _ (Just parent) _ _ _ _) = getMethodSignatureFromST parent
getMethodSignatureFromST _ = Nothing

getMethodSignature :: Semantic (Maybe MethodSig)
getMethodSignature = do
  st <- getSymbolTable
  return $ st >>= getMethodSignatureFromST

getMethodSignature' :: Semantic MethodSig
getMethodSignature' = do
  sig <- getMethodSignature
  case sig of
    Nothing -> throwSemanticException "Cannot find signature for current function!"
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
        sformat ("No symbol table is associated with scope("%int%")!") $ currentScopeID state
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
  Helper functions to manipulate symbol tables.
-}

{- Varaible lookup. -}

lookupLocalVariableFromST :: Name -> SymbolTable -> Maybe (Either Argument FieldDecl)
lookupLocalVariableFromST name st =
  let f = lookupLocalFieldDecl name st
      a = lookupArgument name st
   in (Right <$> f) <|> (Left <$> a)
  where
    lookupLocalFieldDecl name st = Map.lookup name $ variableSymbols st
    lookupArgument name st = do
      let SymbolTable {blockType = btpe} = st
      (MethodSig _ _ args) <- case btpe of
        (MethodBlock sig') -> Just sig'
        _ -> Nothing
      find (\(Argument nm _) -> nm == name) args

lookupVariable :: Name -> Semantic (Maybe (Either Argument FieldDecl))
lookupVariable name = do
  st <- getSymbolTable
  return $ st >>= lookup name
  where
    lookup name st' = (lookupLocalVariableFromST name st') <|> (parent st' >>= lookup name)

lookupVariable' :: Name -> Semantic (Either Argument FieldDecl)
lookupVariable' name = do
  v <- lookupVariable name
  case v of
    Nothing -> throwSemanticException $ sformat ("Varaible "%stext%" not defined") name
    Just v -> return v

{- Method lookup. -}

lookupLocalMethodFromST :: Name -> SymbolTable -> Maybe (Either ImportDecl MethodDecl)
lookupLocalMethodFromST name table =
  let method = do
        methodTable <- methodSymbols table
        Map.lookup name methodTable
      import' = do
        importTable <- importSymbols table
        Map.lookup name importTable
   in (Right <$> method) <|> (Left <$> import')

lookupMethod :: Name -> Semantic (Maybe (Either ImportDecl MethodDecl))
lookupMethod name = do
  lookup name <$> getSymbolTable'
  where
    lookup name table = (lookupLocalMethodFromST name table) <|> (parent table >>= lookup name)

lookupMethod' :: Name -> Semantic (Either ImportDecl MethodDecl)
lookupMethod' name = do
  m <- lookupMethod name
  case m of
    Nothing -> throwSemanticException $ sformat ("Method "%stext%" not found") name
    Just m' -> return m'

{- Add variables and methods -}

addVariableDef :: FieldDecl -> Semantic ()
addVariableDef def = do
  localST <- getSymbolTable'
  -- Semantic[1]
  let nm = name (def :: FieldDecl)
  when
    (isJust (lookupLocalVariableFromST nm localST))
    (addSemanticError $ sformat ("duplicate definition for variable "%stext) $ nm)
  let variableSymbols' = Map.insert (name (def :: FieldDecl)) def (variableSymbols localST)
      newST = localST {variableSymbols = variableSymbols'}
  -- Semantic[4]
  case def of
    (FieldDecl _ _ (Just sz))
      | sz < 0 ->
        addSemanticError $ sformat ("Invalid size of array "%stext) nm
    _ -> return ()
  updateSymbolTable newST

addImportDef :: ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  -- Semantic[1]
  let nm = name (def :: ImportDecl)
  when
    (isJust $ Map.lookup (name (def :: ImportDecl)) importTable)
    (addSemanticError $ sformat ("duplicate import "%stext) nm)
  let importSymbols' = Map.insert (name (def :: ImportDecl)) def importTable
      newST = localST {importSymbols = Just importSymbols'}
  updateSymbolTable newST

addMethodDef :: MethodDecl -> Semantic ()
addMethodDef def = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  -- Semantic[1]
  let nm = name (sig def :: MethodSig)
  when
    (isJust $ lookupLocalMethodFromST nm localST)
    (addSemanticError $ sformat ("duplicate definition for method "%stext) nm)
  let methodSymbols' = Map.insert nm def methodTable
      newST = localST {methodSymbols = Just methodSymbols'}
  updateSymbolTable newST

{-
  Helper methods to do semantic checks.
-}

-- Semantic[8] and Semantic[9]
checkReturnType :: Maybe (WithType Expr) -> Semantic ()
checkReturnType Nothing = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Just t -> addSemanticError $ sformat ("Method "%stext%" expects return type of "%shown%"!") method t
    _ -> return ()
checkReturnType (Just (WithType _ tpe')) = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Nothing -> addSemanticError $ sformat ("Method "%stext%" expects no return value!") method
    t
      | t /= tpe ->
        addSemanticError $
          sformat
            ("Method "%stext%" expects return type of "%shown%", but got "%shown%" instead.")
            method tpe tpe'
    _ -> return ()

-- | Check if content of lit is a valid int64.
-- lit should be striped of whitespace from both ends and contains only
-- numeric characters or the minus sign '-'.
-- -9223372036854775808 ≤ x ≤ 9223372036854775807
-- checks Semantic[22].
checkInt64Literal :: Text -> Semantic Int64
checkInt64Literal lit = do
  when (T.null lit) $
    throwSemanticException "Cannot parse int literal from an empty token!"
  let isNegative = (T.head lit) == '-'
  -- unless
  --   ( (isNegative && (T.drop 1 lit) <= "9223372036854775808")
  --       || (not isNegative && lit <= "9223372036854775807")
  --   )
  --   throwSemanticException
  --   $ printf "Int literal %s is out of bound" lit
  case T.decimal lit of
    Right (n, _) -> return n
    Left msg -> throwSemanticException $ sformat ("cannot parse int literal "%string) msg

checkBoolLiteral :: Text -> Semantic Bool
checkBoolLiteral lit
  | lit == "true" = return True
  | lit == "flase" = return False
  | otherwise = do
    addSemanticError $ sformat ("error parsing bool literal from string "%stext) lit
    return True

checkCharLiteral :: Text -> Semantic Char
checkCharLiteral lit = do
  when
    (T.length lit > 1 || T.null lit)
    (throwSemanticException $ sformat ("cannot parse char literal from string "%stext) lit)
  return $ T.head lit

isInsideLoop :: Semantic Bool
isInsideLoop = do
  lookup <$> getSymbolTable'
  where
    lookup SymbolTable {blockType = ForBlock} = True
    lookup SymbolTable {blockType = WhileBlock} = True
    lookup SymbolTable {blockType = IfBlock, parent = Nothing} = False
    lookup SymbolTable {blockType = IfBlock, parent = Just p} = lookup p

{-
  Methods to generate ir piece by piece.
-}

irgenRoot :: P.Program -> Semantic IRRoot
irgenRoot (P.Program imports fields methods) = do
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
  case mainDecl >>= Just . sig of
    Just (MethodSig _ retType args) -> do
      checkMainRetType retType
      checkMainArgsType args
    Nothing -> return ()
  return $ IRRoot imports' variables' methods'
  where
    checkMainExist main =
      case main of
        Nothing -> do
          addSemanticError "Method \"main\" not found!"
          return Nothing
        Just decl -> return $ Just decl
    checkMainRetType tpe = case tpe of
      Nothing -> return ()
      Just tpe ->
        addSemanticError $
          sformat
            ("Method \"main\" should have return type of void, got "%shown%" instead.") tpe
    checkMainArgsType args =
      unless
        (null args)
        (addSemanticError "Method \"main\" should have no argument.")

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
  return $ importSymbol : rest'

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
      elems <&> \e -> case e of
        (P.WithPos (P.ScalarField id) pos) -> do
          updatePosn pos
          return $ FieldDecl id (irgenType tpe) Nothing
        (P.WithPos (P.VectorField id size) pos) -> do
          updatePosn pos
          sz <- checkInt64Literal size
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
      let sig = MethodSig id (irgenType <$> returnType) args
      block <- irgenBlock (MethodBlock sig) block
      return $ MethodDecl sig block
      where
        args = arguments <&> \(P.WithPos (P.Argument id tpe) _) -> Argument id (irgenType tpe)

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
  let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
  -- Semantic[12]
  case sz of
    Nothing -> return $ WithType (Location id Nothing def) tpe
    Just _ -> return $ WithType (Location id Nothing def) (ArrayType tpe)
irgenLocation (P.VectorLocation id expr) = do
  (WithType expr' indexTpe) <- irgenExpr expr
  -- Semantic[10] (checked in lookupVariable')
  def <- lookupVariable' id
  let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
  -- Semantic[12]
  when (indexTpe /= IntType) (addSemanticError "Index must be of int type!")
  case sz of
    Nothing -> do
      addSemanticError $
        sformat ("Cannot access index of scalar variable "%stext%".") id
      return $ WithType (Location id Nothing def) tpe
    Just _ -> return $ WithType (Location id (Just expr') def) tpe

irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
irgenAssign loc (P.AssignExpr op expr) = do
  loc'@(WithType _ tpe) <- irgenLocation loc
  expr'@(WithType _ tpe') <- irgenExpr expr
  -- Semantic[19]
  when
    (tpe /= tpe')
    (addSemanticError $ sformat ("Assign statement has different types: "%shown%" and "%shown%"") tpe tpe')
  let op' = parseAssignOp op
  -- Semantic[20]
  when
    ((op' == IncAssign || op' == DecAssign) && (tpe /= IntType))
    (addSemanticError "Inc or dec assign only works with int type!")
  return $ Assignment loc' op' (Just expr')
irgenAssign loc (P.IncrementExpr op) = do
  loc'@(WithType _ tpe) <- irgenLocation loc
  let op' = parseAssignOp op
  -- Semantic[20]
  when (tpe /= IntType) (addSemanticError "Inc or dec operator only works on int type!")
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
        _ -> throwSemanticException $ sformat ("method "%stext%" not declared!") method
    Just decl -> case decl of
      Left _ -> return $ MethodCall method argsWithType
      Right m -> do
        let formal = args (sig m :: MethodSig)
        -- Semantic[5] and Semantic[7]
        checkCallingSemantics formal argsWithType
        return $ MethodCall method argsWithType
  where
    matchPred (Argument _ tpe, WithType _ tpe') = tpe == tpe'
    argName (Argument name _, _) = name
    checkArgNum formal args =
      unless
        (length formal == length args)
        ( addSemanticError $
            sformat
              ("Calling "%stext%" with wrong number of args. Required: "%int%", supplied: "%int%".")
              method
              (length formal)
              (length args)
        )
    checkArgType formal args =
      let mismatch = map argName $ filter (not . matchPred) $ zip formal args
       in unless
            (null mismatch)
            ( addSemanticError $
                sformat
                  ("Calling "%stext%" with wrong type of args: "%shown)
                  method
                  mismatch
            )
    arrayOrStringTypePred (WithType _ tpe) = case tpe of
      ArrayType _ -> True
      StringType -> True
      _ -> False
    checkForArrayArg args =
      let arrayArgs = map ele $ filter arrayOrStringTypePred args
       in unless
            (null arrayArgs)
            ( addSemanticError $
                sformat
                  ("Argument of array or string type can not be used for method "%stext)
                  method
            )
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
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of if statment must have type bool, but got "%shown%" instead!") tpe)
  return $ IfStmt expr' ifBlock Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
  ifBlock' <- irgenBlock IfBlock ifBlock
  elseBlock' <- irgenBlock IfBlock elseBlock
  expr'@(WithType _ tpe) <- irgenExpr expr
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of if statment must have type bool, but got "%shown%" instead!") tpe)
  return $ IfStmt expr' ifBlock' (Just elseBlock')
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
  block' <- irgenBlock ForBlock block
  counterExpr' <- irgenExpr counterExpr
  predExpr'@(WithType _ tpe) <- irgenExpr predExpr
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of for statment must have type bool, but got "%shown%" instead!") tpe)
  assign <- irgenAssign loc expr
  return $ ForStmt counter counterExpr' predExpr' assign block'
irgenStmt (P.WhileStatement expr block) = do
  block' <- irgenBlock WhileBlock block
  expr'@(WithType _ tpe) <- irgenExpr expr
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of while statment must have type bool, but got "%shown%" instead!") tpe)
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
  -- Semantic[21]
  inLoop <- isInsideLoop
  unless
    inLoop
    (addSemanticError "Found break statement outside for or while block!")
  return BreakStmt
irgenStmt P.ContinueStatement = do
  -- Semantic[21]
  inLoop <- isInsideLoop
  unless
    inLoop
    (addSemanticError "Found continue statement outside for or while block!")
  return ContinueStmt

{- generate expressions, also do type inference -}
irgenExpr :: P.WithPos P.Expr -> Semantic (WithType Expr)
irgenExpr (P.WithPos (P.LocationExpr loc) pos) = do
  updatePosn pos
  (WithType loc' tpe) <- irgenLocation loc
  return $ WithType (LocationExpr loc') tpe
irgenExpr (P.WithPos (P.MethodCallExpr method@(P.MethodCall name _)) pos) = do
  updatePosn pos
  method' <- irgenMethod method
  m <- lookupMethod' name
  case m of
    -- treat import methods as always return int
    Left _ -> return $ WithType (MethodCallExpr method') IntType
    Right (MethodDecl (MethodSig _ tpe _) _) -> do
      case tpe of
        -- Semantic[6]
        Nothing ->
          throwSemanticException $
            sformat ("Method "%stext%" cannot be used in expressions as it returns nothing!") name
        Just tpe' -> return $ WithType (MethodCallExpr method') tpe'
irgenExpr (P.WithPos (P.IntLiteralExpr i) pos) = do
  updatePosn pos
  literalVal <- checkInt64Literal i
  return $ WithType (IntLiteralExpr literalVal) IntType
irgenExpr (P.WithPos (P.BoolLiteralExpr b) pos) = do
  updatePosn pos
  lit <- checkBoolLiteral b
  return $ WithType (BoolLiteralExpr lit) BoolType
irgenExpr (P.WithPos (P.CharLiteralExpr c) pos) = do
  updatePosn pos
  lit <- checkCharLiteral c
  return $ WithType (CharLiteralExpr lit) IntType
irgenExpr (P.WithPos (P.LenExpr id) pos) = do
  updatePosn pos
  def <- lookupVariable' id
  -- Semantic[13]
  case def of
    Left (Argument nm _) -> addSemanticError $ sformat ("len cannot operate on argument "%stext%"!") nm
    Right (FieldDecl nm _ sz) ->
      when
        (isNothing sz)
        (addSemanticError $ sformat ("len cannot operate on scalar variable "%stext%"!") nm)
  return $ WithType (LengthExpr id) IntType
irgenExpr (P.WithPos (P.ArithOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[16]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  when
    (ltp /= IntType || rtp /= IntType)
    (addSemanticError "There can only be integer values in arithmetic expressions.")
  return $ WithType (ArithOpExpr (parseArithOp op) l' r') IntType
irgenExpr (P.WithPos (P.RelOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[16]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  when
    (ltp /= IntType || rtp /= IntType)
    (addSemanticError "There can only be integer values in relational expressions.")
  return $ WithType (RelOpExpr (parseRelOp op) l' r') IntType
irgenExpr (P.WithPos (P.EqOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[17]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  when
    ((ltp, rtp) /= (IntType, IntType) && (ltp, rtp) /= (BoolType, BoolType))
    (addSemanticError "Can only check equality of expressions with the SAME type!")
  return $ WithType (EqOpExpr (parseEqOp op) l' r') BoolType
irgenExpr (P.WithPos (P.CondOpExpr op l r) pos) = do
  updatePosn pos
  -- Semantic[18]
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  when
    (ltp /= BoolType || rtp /= BoolType)
    (addSemanticError "Conditional ops only accept booleans!")
  return $ WithType (CondOpExpr (parseCondOp op) l' r') BoolType
irgenExpr (P.WithPos (P.NegativeExpr expr) pos) = do
  updatePosn pos
  -- Semantic[16]
  expr'@(WithType _ tpe) <- irgenExpr expr
  when
    (tpe /= IntType)
    (addSemanticError "Operator \"-\" only accepts integers!")
  return $ WithType (NegOpExpr Neg expr') IntType
irgenExpr (P.WithPos (P.NegateExpr expr) pos) = do
  updatePosn pos
  -- Semantic[18]
  expr'@(WithType _ tpe) <- irgenExpr expr
  when
    (tpe /= BoolType)
    (addSemanticError "Operator \"!\" only accepts integers!")
  return $ WithType (NotOpExpr Not expr') BoolType
irgenExpr (P.WithPos (P.ChoiceExpr pred l r) pos) = do
  updatePosn pos
  pred'@(WithType _ ptp) <- irgenExpr pred
  l'@(WithType _ ltp) <- irgenExpr l
  r'@(WithType _ rtp) <- irgenExpr r
  -- Semantic[15]
  when
    (ptp /= BoolType)
    (addSemanticError "Predicate of choice operator must be a boolean!")
  when
    (ltp /= rtp)
    (addSemanticError "Alternatives of choice op should have same type!")
  return $ WithType (ChoiceOpExpr Choice pred' l' r') ltp
irgenExpr (P.WithPos (P.ParenExpr expr) pos) = do
  updatePosn pos
  irgenExpr expr

irgenImportArg :: P.ImportArg -> Semantic (WithType Expr)
irgenImportArg (P.ExprImportArg expr) = irgenExpr expr
irgenImportArg (P.StringImportArg (P.WithPos arg pos)) = do
  updatePosn pos
  return $ WithType (StringLiteralExpr arg) StringType
