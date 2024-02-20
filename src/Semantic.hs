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
module Semantic
  ( runSemanticAnalysis,
    SymbolTable (..),
    ScopeID,
    BlockType (..),
    lookupLocalVariableFromST,
    lookupLocalMethodFromST,
  )
where

import AST
import Constants
import Control.Applicative ((<|>))
import Control.Lens (view, (^.))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Formatting
import GHC.Generics (Generic)
import Parser qualified as P
import Types
import Util.SourceLoc qualified as SL

---------------------------------------
-- Semantic informations and errors
---------------------------------------

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
data SemanticError = SemanticError SL.Range Text

instance Show SemanticError where
  show (SemanticError range msg) = formatToString (shown % ": " % stext) range msg

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
data SemanticException = SemanticException SL.Range Text

instance Show SemanticException where
  show (SemanticException range msg) = formatToString (shown % " " % stext) range msg

data BlockType = RootBlock | IfBlock | ForBlock | WhileBlock | MethodBlock
  deriving (Show, Eq)

-- symbol table definitions
data SymbolTable = SymbolTable
  { scopeID :: ScopeID,
    parent :: Maybe SymbolTable,
    importSymbols :: Maybe (Map Name ImportDecl),
    variableSymbols :: Map Name FieldDecl,
    methodSymbols :: Maybe (Map Name MethodDecl),
    blockType :: BlockType,
    methodSig :: Maybe MethodSig
  }
  deriving (Generic)

instance Show SymbolTable where
  show (SymbolTable sid p imports variables methods tpe _) =
    formatToString
      ("SymbolTable {scopeID=" % int % ", parent=" % shown % ", imports=" % shown % ", variables=" % shown % ", methods=" % shown % ", tpe=" % shown)
      sid
      (scopeID <$> p)
      imports
      variables
      methods
      tpe

data SemanticState = SemanticState
  { nextScopeID :: ScopeID,
    currentScopeID :: ScopeID,
    symbolTables :: Map ScopeID SymbolTable,
    currentRange :: SL.Range
  }
  deriving (Show)

-- Monad used for semantic analysis
-- Symbol tables are built for every scope, and stored in SemanticState.
-- Semantic errors encountered are recorded by the writer monad (WriterT [SemanticError]).
-- If a serious problem happened such that the analysis has to be aborted, a SemanticException
-- is thrown.
newtype Semantic a = Semantic {runSemantic :: ExceptT SemanticException (WriterT [SemanticError] (State SemanticState)) a}
  deriving (Functor, Applicative, Monad, MonadError SemanticException, MonadWriter [SemanticError], MonadState SemanticState)

runSemanticAnalysis :: P.Program -> Either String (ASTRoot, [SemanticError], Map ScopeID SymbolTable)
runSemanticAnalysis p =
  let ir = irgenRoot p
      ((except, errors), state) = (runState $ runWriterT $ runExceptT $ runSemantic ir) initialSemanticState
   in case except of
        Left except -> Left $ show except
        Right a -> Right (a, errors, symbolTables state)

initialSemanticState :: SemanticState
initialSemanticState =
  let globalST =
        SymbolTable
          { scopeID = globalScopeID,
            parent = Nothing,
            importSymbols = Just Map.empty,
            variableSymbols = Map.empty,
            methodSymbols = Just Map.empty,
            blockType = RootBlock,
            methodSig = Nothing
          }
   in SemanticState
        { nextScopeID = globalScopeID + 1,
          currentScopeID = globalScopeID,
          symbolTables = Map.fromList [(globalScopeID, globalST)],
          currentRange = SL.Range (SL.Posn 0 0 0) (SL.Posn 0 0 0)
        }

updateCurrentRange :: SL.Range -> Semantic ()
updateCurrentRange range = modify (\s -> s {currentRange = range})

getCurrentRange :: Semantic SL.Range
getCurrentRange = gets currentRange

-- throw exception or store errors
throwSemanticException :: Text -> Semantic a
throwSemanticException msg = do
  range <- getCurrentRange
  throwError $ SemanticException range msg

addSemanticError :: Text -> Semantic ()
addSemanticError msg = do
  range <- getCurrentRange
  tell [SemanticError range msg]

-- find symbol table for global scope
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
      throwSemanticException $ sformat ("No symble table found for current scope " % int) scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map Name FieldDecl)
getLocalVariables' = do
  variableSymbols <$> getSymbolTable'

getLocalImports' :: Semantic (Map Name ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case importSymbols localST of
    Nothing -> throwSemanticException $ sformat ("No import table for scope " % int) $ scopeID localST
    Just t -> return t

getLocalMethods' :: Semantic (Map Name MethodDecl)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methodSymbols localST of
    Nothing -> throwSemanticException $ sformat ("No method table for scope " % int) $ scopeID localST
    Just t -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state {symbolTables = Map.insert (currentScopeID state) t (symbolTables state)}

getMethodSignature :: Semantic (Maybe MethodSig)
getMethodSignature = do lookup <$> getSymbolTable'
  where
    lookup :: SymbolTable -> Maybe MethodSig
    lookup SymbolTable {blockType = RootBlock} = Nothing
    lookup SymbolTable {blockType = MethodBlock, methodSig = sig} = sig
    lookup SymbolTable {parent = Just p} = lookup p

getMethodSignature' :: Semantic MethodSig
getMethodSignature' = do
  sig <- getMethodSignature
  case sig of
    Nothing -> throwSemanticException "Cannot find signature for current function!"
    Just s -> return s

enterScope :: BlockType -> Maybe MethodSig -> Semantic ScopeID
enterScope blockType sig = do
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
            blockType = blockType,
            methodSig = sig
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
        sformat ("No symbol table is associated with scope(" % int % ")!") $
          currentScopeID state
    Just table ->
      case parent table of
        Nothing ->
          throwSemanticException "Cannot exit root scope!"
        Just p ->
          put $ state {currentScopeID = scopeID p}

----------------------------------------------------------------------
-- Convert the parser tree into an AST
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

lookupLocalVariableFromST :: Name -> SymbolTable -> Maybe MethodSig -> Maybe (Either Argument FieldDecl)
lookupLocalVariableFromST name st sig =
  let f = lookupLocalFieldDecl name st
      a = lookupArgument name st
   in (Right <$> f) <|> (Left <$> a)
  where
    lookupLocalFieldDecl name st = Map.lookup name $ variableSymbols st
    lookupArgument name st = do
      (MethodSig _ _ args) <- sig
      find (\(Argument{name=nm}) -> nm == name) args

lookupVariable :: Name -> Semantic (Maybe (Either Argument FieldDecl))
lookupVariable name = do
  st <- getSymbolTable
  sig <- getMethodSignature
  return $ st >>= lookup name sig
  where
    lookup name sig st' =
      (lookupLocalVariableFromST name st' sig)
        <|> (parent st' >>= lookup name sig)

lookupVariable' :: Name -> Semantic (Either Argument FieldDecl)
lookupVariable' name = do
  v <- lookupVariable name
  case v of
    Nothing -> throwSemanticException $ sformat ("Varaible " % stext % " not defined") name
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
    Nothing -> throwSemanticException $ sformat ("Method " % stext % " not found") name
    Just m' -> return m'

{- Add variables and methods -}

addVariableDef :: FieldDecl -> Semantic ()
addVariableDef def = do
  localST <- getSymbolTable'
  sig <- getMethodSignature
  -- Semantic[1]
  let nm = view #name def
  when
    (isJust (lookupLocalVariableFromST nm localST sig))
    (addSemanticError $ sformat ("duplicate definition for variable " % stext) nm)
  let variableSymbols' = Map.insert nm def (variableSymbols localST)
      newST = localST {variableSymbols = variableSymbols'}
  -- Semantic[4]
  case def of
    (FieldDecl _ (ArrayType _ sz) _)
      | sz < 0 ->
          addSemanticError $ sformat ("Invalid size of array " % stext) nm
    _ -> return ()
  updateSymbolTable newST

addImportDef :: ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  -- Semantic[1]
  let nm = view #name def
  when
    (isJust $ Map.lookup nm importTable)
    (addSemanticError $ sformat ("duplicate import " % stext) nm)
  let importSymbols' = Map.insert nm def importTable
      newST = localST {importSymbols = Just importSymbols'}
  updateSymbolTable newST

addMethodDef :: MethodDecl -> Semantic ()
addMethodDef def = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  -- Semantic[1]
  let nm = def ^. (#sig . #name)
  when
    (isJust $ lookupLocalMethodFromST nm localST)
    (addSemanticError $ sformat ("duplicate definition for method " % stext) nm)
  let methodSymbols' = Map.insert nm def methodTable
      newST = localST {methodSymbols = Just methodSymbols'}
  updateSymbolTable newST

{-
  Helper methods to do semantic checks.
-}

-- Semantic[8] and Semantic[9]
checkReturnType :: Maybe Expr -> Semantic ()
checkReturnType Nothing = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Just t -> addSemanticError $ sformat ("Method " % stext % " expects return type of " % shown % "!") method t
    _ -> return ()
checkReturnType (Just Expr{tpe = tpe'}) = do
  (MethodSig method tpe _) <- getMethodSignature'
  case tpe of
    Nothing -> addSemanticError $ sformat ("Method " % stext % " expects no return value!") method
    t
      | t /= tpe ->
          addSemanticError $
            sformat
              ("Method " % stext % " expects return type of " % shown % ", but got " % shown % " instead.")
              method
              tpe
              tpe'
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
    Left msg -> throwSemanticException $ sformat ("cannot parse int literal " % string) msg

checkBoolLiteral :: Text -> Semantic Bool
checkBoolLiteral lit
  | lit == "true" = return True
  | lit == "false" = return False
  | otherwise = do
      addSemanticError $ sformat ("error parsing bool literal from string " % stext) lit
      return True

checkCharLiteral :: Text -> Semantic Char
checkCharLiteral lit = do
  when
    (T.length lit > 1 || T.null lit)
    (throwSemanticException $ sformat ("cannot parse char literal from string " % stext) lit)
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

irgenRoot :: P.Program -> Semantic ASTRoot
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
  case mainDecl >>= Just . view #sig of
    Just (MethodSig _ retType args) -> do
      checkMainRetType retType
      checkMainArgsType args
    Nothing -> return ()
  return $ ASTRoot imports' variables' methods'
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
            ("Method \"main\" should have return type of void, got " % shown % " instead.")
            tpe
    checkMainArgsType args =
      unless
        (null args)
        (addSemanticError "Method \"main\" should have no argument.")

irgenType :: P.Type -> Type
irgenType P.IntType = IntType
irgenType P.BoolType = BoolType

irgenImports :: [SL.Located P.ImportDecl] -> Semantic [ImportDecl]
irgenImports [] = return []
irgenImports ((SL.LocatedAt range (P.ImportDecl id)) : rest) = do
  let importSymbol = ImportDecl id range
  addImportDef importSymbol
  -- TODO: This kind of recursions potentially lead to stack overflows.
  -- For now it should do the job. Will try to fix in the future.
  rest' <- irgenImports rest
  return $ importSymbol : rest'

irgenFieldDecls :: [SL.Located P.FieldDecl] -> Semantic [FieldDecl]
irgenFieldDecls [] = return []
irgenFieldDecls ((SL.LocatedAt pos decl) : rest) = do
  fields <- sequence $ convertFieldDecl decl
  vars <- addVariables fields
  rest' <- irgenFieldDecls rest
  return (vars ++ rest')
  where
    convertFieldDecl (P.FieldDecl tpe elems) =
      elems <&> \e -> case e of
        (SL.LocatedAt range (P.ScalarField id)) -> do
          updateCurrentRange range
          return $ FieldDecl id (irgenType tpe) pos
        (SL.LocatedAt range (P.VectorField id size)) -> do
          updateCurrentRange pos
          sz <- checkInt64Literal size
          return $ FieldDecl id (ArrayType (irgenType tpe) sz) pos
    addVariables [] = return []
    addVariables (v : vs) = do
      addVariableDef v
      vs' <- addVariables vs
      return (v : vs')

irgenMethodDecls :: [SL.Located P.MethodDecl] -> Semantic [MethodDecl]
irgenMethodDecls [] = return []
irgenMethodDecls ((SL.LocatedAt range decl) : rest) = do
  updateCurrentRange range
  method <- convertMethodDecl decl
  -- Semantic[8] and Semantic[9]
  -- checkMethod method
  addMethodDef method
  rest' <- irgenMethodDecls rest
  return (method : rest')
  where
    convertMethodDecl (P.MethodDecl id returnType arguments block) = do
      let sig = MethodSig id (irgenType <$> returnType) args
      block <- irgenBlock MethodBlock (Just sig) block
      return $ MethodDecl sig block range
      where
        args =
          arguments <&> \(SL.LocatedAt range (P.Argument id tpe)) ->
            Argument id (irgenType tpe) range

irgenBlock :: BlockType -> Maybe MethodSig -> P.Block -> Semantic Block
irgenBlock blockType sig (P.Block fieldDecls statements) = do
  nextID <- enterScope blockType sig
  fields <- irgenFieldDecls fieldDecls
  stmts <- irgenStatements statements
  let block = Block fields stmts nextID
  exitScope
  return block

irgenLocation :: P.Location -> Semantic Location
irgenLocation (P.ScalarLocation id) = do
  -- Semantic[10] (checked in lookupVariable')
  def <- lookupVariable' id
  range <- getCurrentRange
  let tpe = either (\(Argument _ tpe' _) -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz =
        either
          (const Nothing)
          ( \(FieldDecl _ tpe _) -> case tpe of
              (ArrayType _ sz') -> Just sz'
              _ -> Nothing
          )
          def
  -- Semantic[12]
  case sz of
    Nothing -> return $ Location id Nothing def tpe range
    Just v -> return $ Location id Nothing def (ArrayType tpe v) range
irgenLocation (P.VectorLocation id expr) = do
  expr'@(Expr _ indexTpe _) <- irgenExpr expr
  -- Semantic[10] (checked in lookupVariable')
  def <- lookupVariable' id
  range <- getCurrentRange
  let tpe = either (\(Argument _ tpe' _) -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz =
        either
          (const Nothing)
          ( \(FieldDecl _ tpe _) -> case tpe of
              (ArrayType _ sz') -> Just sz'
              _ -> Nothing
          )
          def
  -- Semantic[12]
  when (indexTpe /= IntType) (addSemanticError "Index must be of int type!")
  case sz of
    Nothing -> do
      addSemanticError $
        sformat ("Cannot access index of scalar variable " % stext % ".") id
      return $ Location id Nothing def tpe range
    Just _ -> return $ Location id (Just expr') def tpe range

checkAssignType :: Type -> Type -> Bool
checkAssignType (ArrayType tpe _) exprType = tpe == exprType
checkAssignType locType exprType = locType == exprType

irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
irgenAssign loc (P.AssignExpr op expr) = do
  loc'@Location{tpe=tpe} <- irgenLocation loc
  expr'@Expr{tpe=tpe'} <- irgenExpr expr
  range <- getCurrentRange
  -- Semantic[19]
  unless 
    (checkAssignType tpe tpe')
    (addSemanticError $ sformat ("Assign statement has different types: " % shown % " and " % shown % "") tpe tpe')
  let op' = parseAssignOp op
  -- Semantic[20]
  when
    ((op' == IncAssign || op' == DecAssign) && (tpe /= IntType))
    (addSemanticError "Inc or dec assign only works with int type!")
  return $ Assignment loc' op' (Just expr') range
irgenAssign loc (P.IncrementExpr op) = do
  loc'@Location{tpe=tpe} <- irgenLocation loc
  range <- getCurrentRange
  let op' = parseAssignOp op
  -- Semantic[20]
  when (tpe /= IntType) (addSemanticError "Inc or dec operator only works on int type!")
  return $ Assignment loc' op' Nothing range

irgenStatements :: [SL.Located P.Statement] -> Semantic [Statement]
irgenStatements [] = return []
irgenStatements ((SL.LocatedAt range s) : xs) = do
  updateCurrentRange range
  s' <- irgenStmt s
  xs' <- irgenStatements xs
  return (s' : xs')

irgenMethod :: P.MethodCall -> Semantic MethodCall
irgenMethod (P.MethodCall method args') = do
  -- Semantic[2] and Semantic[11]
  decl' <- lookupMethod method
  argsTyped <- traverse irgenImportArg args'
  range <- getCurrentRange
  case decl' of
    Nothing -> do
      currentMethod <- getMethodSignature
      case currentMethod of
        -- Recursive method calling itself
        (Just (MethodSig name _ formal)) | name == method -> do
          checkCallingSemantics formal argsTyped
          return $ MethodCall method argsTyped range
        _ -> throwSemanticException $ sformat ("method " % stext % " not declared!") method
    Just decl -> case decl of
      Left _ -> return $ MethodCall method argsTyped range
      Right m -> do
        let formal = m ^. (#sig . #args)
        -- Semantic[5] and Semantic[7]
        checkCallingSemantics formal argsTyped
        return $ MethodCall method argsTyped range
  where
    matchPred (Argument _ tpe _, Expr{tpe=tpe'}) = tpe == tpe'
    argName (Argument name _ _, _) = name
    checkArgNum formal args =
      unless
        (length formal == length args)
        ( addSemanticError $
            sformat
              ("Calling " % stext % " with wrong number of args. Required: " % int % ", supplied: " % int % ".")
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
                  ("Calling " % stext % " with wrong type of args: " % shown)
                  method
                  mismatch
            )
    arrayOrStringTypePred Expr{tpe=tpe} = case tpe of
      ArrayType _ _ -> True
      StringType -> True
      _ -> False
    checkForArrayArg args =
      let arrayArgs = filter arrayOrStringTypePred args
       in unless
            (null arrayArgs)
            ( addSemanticError $
                sformat
                  ("Argument of array or string type can not be used for method " % stext)
                  method
            )
    checkCallingSemantics formal args = do
      checkArgNum formal args
      checkArgType formal args
      checkForArrayArg args

irgenStmt :: P.Statement -> Semantic Statement
irgenStmt (P.AssignStatement loc expr) = do
  assign <- irgenAssign loc expr
  range <- getCurrentRange
  return $ Statement (AssignStmt assign) range
irgenStmt (P.MethodCallStatement method) = do
  method' <- irgenMethod method
  range <- getCurrentRange
  return $ Statement (MethodCallStmt method') range
irgenStmt (P.IfStatement expr block) = do
  ifBlock <- irgenBlock IfBlock Nothing block
  expr'@Expr{tpe=tpe} <- irgenExpr expr
  range <- getCurrentRange
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of if statment must have type bool, but got " % shown % " instead!") tpe)
  return $ Statement (IfStmt expr' ifBlock Nothing) range
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
  ifBlock' <- irgenBlock IfBlock Nothing ifBlock
  elseBlock' <- irgenBlock IfBlock Nothing elseBlock
  expr'@Expr{tpe=tpe} <- irgenExpr expr
  range <- getCurrentRange
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of if statment must have type bool, but got " % shown % " instead!") tpe)
  return $ Statement (IfStmt expr' ifBlock' (Just elseBlock')) range
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
  block' <- irgenBlock ForBlock Nothing block
  counterExpr' <- irgenExpr counterExpr
  predExpr'@Expr{tpe=tpe} <- irgenExpr predExpr
  range <- getCurrentRange
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of for statment must have type bool, but got " % shown % " instead!") tpe)
  assign <- irgenAssign loc expr
  return $ Statement (ForStmt counter counterExpr' predExpr' assign block') range
irgenStmt (P.WhileStatement expr block) = do
  block' <- irgenBlock WhileBlock Nothing block
  expr'@Expr{tpe=tpe} <- irgenExpr expr
  range <- getCurrentRange
  -- Semantic[14]
  when
    (tpe /= BoolType)
    (addSemanticError $ sformat ("The pred of while statment must have type bool, but got " % shown % " instead!") tpe)
  return $ Statement (WhileStmt expr' block') range
irgenStmt (P.ReturnExprStatement expr) = do
  expr' <- irgenExpr expr
  range <- getCurrentRange
  -- Semantic[8] and Semantic[9]
  checkReturnType $ Just expr'
  return $ Statement (ReturnStmt $ Just expr') range
irgenStmt P.ReturnVoidStatement = do
  range <- getCurrentRange
  -- Semantic[8] and Semantic[9]
  checkReturnType Nothing
  return $ Statement (ReturnStmt Nothing) range
irgenStmt P.BreakStatement = do
  range <- getCurrentRange
  -- Semantic[21]
  inLoop <- isInsideLoop
  unless
    inLoop
    (addSemanticError "Found break statement outside for or while block!")
  return $ Statement BreakStmt range
irgenStmt P.ContinueStatement = do
  range <- getCurrentRange
  -- Semantic[21]
  inLoop <- isInsideLoop
  unless
    inLoop
    (addSemanticError "Found continue statement outside for or while block!")
  return $ Statement ContinueStmt range

{- generate expressions, also do type inference -}
irgenExpr :: SL.Located P.Expr -> Semantic Expr
irgenExpr (SL.LocatedAt range (P.LocationExpr loc)) = do
  updateCurrentRange range
  loc'@Location{tpe=tpe} <- irgenLocation loc
  return $ Expr (LocationExpr loc') tpe range 
irgenExpr (SL.LocatedAt range (P.MethodCallExpr method@(P.MethodCall name _))) = do
  updateCurrentRange range
  method' <- irgenMethod method
  m <- lookupMethod' name
  case m of
    -- treat import methods as always return int
    Left _ -> return $ Expr (MethodCallExpr method') IntType range
    Right (MethodDecl (MethodSig _ tpe _) _ _) -> do
      case tpe of
        -- Semantic[6]
        Nothing ->
          throwSemanticException $
            sformat ("Method " % stext % " cannot be used in expressions as it returns nothing!") name
        Just tpe' -> return $ Expr (MethodCallExpr method') tpe' range
irgenExpr (SL.LocatedAt range (P.IntLiteralExpr i)) = do
  updateCurrentRange range
  literalVal <- checkInt64Literal i
  return $ Expr (IntLiteralExpr literalVal) IntType range
irgenExpr (SL.LocatedAt range (P.BoolLiteralExpr b)) = do
  updateCurrentRange range
  lit <- checkBoolLiteral b
  return $ Expr (BoolLiteralExpr lit) BoolType range
irgenExpr (SL.LocatedAt range (P.CharLiteralExpr c)) = do
  updateCurrentRange range
  lit <- checkCharLiteral c
  return $ Expr (CharLiteralExpr lit) IntType range
irgenExpr (SL.LocatedAt range (P.LenExpr id)) = do
  updateCurrentRange range
  def <- lookupVariable' id
  -- Semantic[13]
  case def of
    Left (Argument nm _ _) -> addSemanticError $ sformat ("len cannot operate on argument " % stext % "!") nm
    Right (FieldDecl nm (ArrayType _ _) _) -> return ()
    Right (FieldDecl nm _ _) -> addSemanticError $ sformat ("len cannot operate on scalar variable " % stext % "!") nm
  return $ Expr (LengthExpr id) IntType range
irgenExpr (SL.LocatedAt range (P.ArithOpExpr op l r)) = do
  updateCurrentRange range
  -- Semantic[16]
  l'@Expr{tpe=ltp} <- irgenExpr l
  r'@Expr{tpe=rtp} <- irgenExpr r
  when
    (ltp /= IntType || rtp /= IntType)
    (addSemanticError "There can only be integer values in arithmetic expressions.")
  return $ Expr (ArithOpExpr (parseArithOp op) l' r') IntType range
irgenExpr (SL.LocatedAt range (P.RelOpExpr op l r)) = do
  updateCurrentRange range
  -- Semantic[16]
  l'@Expr{tpe=ltp} <- irgenExpr l
  r'@Expr{tpe=rtp} <- irgenExpr r
  when
    (ltp /= IntType || rtp /= IntType)
    (addSemanticError "There can only be integer values in relational expressions.")
  return $ Expr (RelOpExpr (parseRelOp op) l' r') IntType range
irgenExpr (SL.LocatedAt range (P.EqOpExpr op l r)) = do
  updateCurrentRange range
  -- Semantic[17]
  l'@Expr{tpe=ltp} <- irgenExpr l
  r'@Expr{tpe=rtp} <- irgenExpr r
  when
    ((ltp, rtp) /= (IntType, IntType) && (ltp, rtp) /= (BoolType, BoolType))
    (addSemanticError "Can only check equality of expressions with the SAME type!")
  return $ Expr (EqOpExpr (parseEqOp op) l' r') BoolType range
irgenExpr (SL.LocatedAt range (P.CondOpExpr op l r)) = do
  updateCurrentRange range
  -- Semantic[18]
  l'@Expr{tpe=ltp} <- irgenExpr l
  r'@Expr{tpe=rtp} <- irgenExpr r
  when
    (ltp /= BoolType || rtp /= BoolType)
    (addSemanticError "Conditional ops only accept booleans!")
  return $ Expr (CondOpExpr (parseCondOp op) l' r') BoolType range
irgenExpr (SL.LocatedAt range (P.NegativeExpr expr)) = do
  updateCurrentRange range
  -- Semantic[16]
  expr'@Expr{tpe=tpe} <- irgenExpr expr
  when
    (tpe /= IntType)
    (addSemanticError "Operator \"-\" only accepts integers!")
  return $ Expr (NegOpExpr Neg expr') IntType range
irgenExpr (SL.LocatedAt range (P.NegateExpr expr)) = do
  updateCurrentRange range
  -- Semantic[18]
  expr'@Expr{tpe=tpe} <- irgenExpr expr
  when
    (tpe /= BoolType)
    (addSemanticError "Operator \"!\" only accepts integers!")
  return $ Expr (NotOpExpr Not expr') BoolType range
irgenExpr (SL.LocatedAt range (P.ChoiceExpr pred l r)) = do
  updateCurrentRange range
  pred'@Expr{tpe=ptp} <- irgenExpr pred
  l'@Expr{tpe=ltp} <- irgenExpr l
  r'@Expr{tpe=rtp} <- irgenExpr r
  -- Semantic[15]
  when
    (ptp /= BoolType)
    (addSemanticError "Predicate of choice operator must be a boolean!")
  when
    (ltp /= rtp)
    (addSemanticError "Alternatives of choice op should have same type!")
  return $ Expr (ChoiceOpExpr Choice pred' l' r') ltp range
irgenExpr (SL.LocatedAt range (P.ParenExpr expr)) = do
  updateCurrentRange range
  irgenExpr expr

irgenImportArg :: SL.Located P.ImportArg -> Semantic Expr
irgenImportArg (SL.LocatedAt range (P.ExprImportArg expr)) = irgenExpr expr
irgenImportArg (SL.LocatedAt range (P.StringImportArg arg)) = do
  updateCurrentRange range
  return $ Expr (StringLiteralExpr arg) StringType range
