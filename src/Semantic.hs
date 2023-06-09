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
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (find, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Read as T
import Formatting
import IR
import qualified Parser as P

---------------------------------------
-- Semantic informations and errors
---------------------------------------

-- semantic errors
-- these errors are produced during semantic analysis,
-- we try to detect as many as we can in a single pass
data SemanticError = SemanticError P.Posn Text

instance Show SemanticError where
  show (SemanticError (P.Posn row col) msg) = formatToString ("[" % int % ":" % int % "] " % stext) row col msg

-- exceptions during semantic analysis
-- difference from SemanticError:
-- whenever an exception is raised, the analysis procedure will be aborted.
data SemanticException = SemanticException P.Posn Text

instance Show SemanticException where
  show (SemanticException (P.Posn row col) msg) = formatToString ("[" % int % ":" % int % "] " % stext) row col msg

data BlockType = RootBlock | IfBlock | ForBlock | WhileBlock | MethodBlock MethodSig
  deriving (Show, Eq)

data ImportDecl = ImportDecl {name :: Name}
  deriving (Show)

data FieldDecl = FieldDecl
  { name :: Name,
    tpe :: Type,
    size :: Maybe Int64
  }
  deriving (Show)

data Argument = Argument
  { name :: Name,
    tpe :: Type
  }
  deriving (Show, Eq)

data MethodSig = MethodSig
  { name :: Name,
    tpe :: Maybe Type,
    args :: [Argument]
  }
  deriving (Show, Eq)

-- symbol table definitions
data SymbolTable = SymbolTable
  { scopeID :: ScopeID,
    parent :: Maybe SymbolTable,
    variableTemporals :: Map Name [Address],
    variableDecls :: Map Name FieldDecl,
    imports :: Maybe (Map Name ImportDecl),
    methods :: Maybe (Map Name MethodSig),
    blockType :: BlockType
  }

instance Show SymbolTable where
  show (SymbolTable sid p _ variables imports methods tpe) =
    formatToString
      ( "SymbolTable {scopeID=" % int % ", parent=" % build % ", imports=" % string
          % ", variables="
          % string
          % ", methods="
          % string
          % ", blockType="
          % string
          % "}"
      )
      sid
      (scopeID <$> p)
      (show imports)
      (show variables)
      (show methods)
      (show tpe)

data MethodInstrs = MethodInstrs
  { methodSig :: MethodSig,
    instrs :: [IR.IRInstruction]
  }

instance Show MethodInstrs where
  show (MethodInstrs sig instrs) = show sig ++ "\n[\n" ++ (intercalate "\n" (show <$> instrs)) ++ "\n]\n"

data SemanticState = SemanticState
  { nextScopeID :: ScopeID,
    currentScopeID :: ScopeID,
    nextSymbolID :: SymbolID,
    symbolTables :: Map ScopeID SymbolTable,
    currentPosn :: P.Posn,
    currentInstrs :: [IR.IRInstruction]
  }
  deriving (Show)

-- Monad used for semantic analysis
-- Symbol tables are built for every scope, and stored in SemanticState.
-- Semantic errors encountered are recorded by the writer monad (WriterT [SemanticError]).
-- If a serious problem happened such that the analysis has to be aborted, a SemanticException
-- is thrown.
newtype Semantic a = Semantic {runSemantic :: StateT SemanticState (WriterT ([SemanticError], [MethodInstrs]) (Except SemanticException)) a}
  deriving (Functor, Applicative, Monad, MonadError SemanticException, MonadWriter ([SemanticError], [MethodInstrs]), MonadState SemanticState)

instance MonadFail Semantic where
  fail s = throwSemanticException ("Unknown error: " % string) s

runSemanticAnalysis :: P.Program -> Either String ([SemanticError], [MethodInstrs])
runSemanticAnalysis p =
  let ir = irgenRoot p
      exceptOrResult = (runExcept $ runWriterT $ runStateT (runSemantic ir) initialSemanticState)
   in case exceptOrResult of
        Left except -> Left $ show except
        Right ((_, state), (errors, instrs)) -> Right (errors, instrs)

initialSemanticState :: SemanticState
initialSemanticState =
  let globalST =
        SymbolTable
          { scopeID = globalScopeID,
            parent = Nothing,
            imports = Just Map.empty,
            variableTemporals = Map.empty,
            variableDecls = Map.empty,
            methods = Just Map.empty,
            blockType = RootBlock
          }
   in SemanticState
        { nextScopeID = globalScopeID + 1,
          currentScopeID = globalScopeID,
          nextSymbolID = 0,
          symbolTables = Map.fromList [(globalScopeID, globalST)],
          currentPosn = P.Posn {row = 0, col = 0},
          currentInstrs = []
        }

-- writer monad manipulation
tellErrors :: [SemanticError] -> Semantic ()
tellErrors errors = tell (errors, mempty)

tellInstrs :: [MethodInstrs] -> Semantic ()
tellInstrs instrs = tell (mempty, instrs)

-- throw exception
throwSemanticException :: Format (Semantic a) b -> b
throwSemanticException m =
  runFormat
    m
    ( \builder -> do
        posn <- getPosn
        throwError $ SemanticException posn (TL.toStrict $ TLB.toLazyText builder)
    )

addSemanticError :: Format (Semantic ()) a -> a
addSemanticError m = do
  runFormat
    m
    ( \builder -> do
        posn <- getPosn
        tellErrors [SemanticError posn (TL.toStrict $ TLB.toLazyText builder)]
    )

-- get or update position information
updatePosn :: P.Posn -> Semantic ()
updatePosn posn = do
  st <- get
  put st {currentPosn = posn}

getPosn :: Semantic P.Posn
getPosn = do
  SemanticState {currentPosn = posn} <- get
  return posn

-- manipulate instructions
addInstructions :: [IRInstruction] -> Semantic ()
addInstructions instrs = do
  modify $ \s -> s {currentInstrs = currentInstrs s ++ instrs}

offloadInstructions :: Semantic [IRInstruction]
offloadInstructions = do
  instrs <- gets currentInstrs
  modify $ \s -> s {currentInstrs = []}
  return instrs

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
      throwSemanticException ("No symble table found for current scope " % int) scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map Name FieldDecl)
getLocalVariables' = do
  variableDecls <$> getSymbolTable'

getLocalImports' :: Semantic (Map Name ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case imports localST of
    Nothing -> throwSemanticException ("No import table for scope " % int) (scopeID localST)
    Just t -> return t

getLocalMethods' :: Semantic (Map Name MethodSig)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methods localST of
    Nothing -> throwSemanticException ("No method table for scope " % int) (scopeID localST)
    Just t -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state {symbolTables = Map.insert (currentScopeID state) t (symbolTables state)}

getMethodSignatureFromST :: SymbolTable -> Maybe MethodSig
getMethodSignatureFromST SymbolTable {blockType = RootBlock} = Nothing
getMethodSignatureFromST SymbolTable {blockType = (MethodBlock sig)} = Just sig
getMethodSignatureFromST SymbolTable {parent = (Just parent)} = getMethodSignatureFromST parent
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
            variableTemporals = Map.empty,
            variableDecls = Map.empty,
            imports = Nothing,
            methods = Nothing,
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
      throwSemanticException
        ("No symbol table is associated with scope(" % int % ")!")
        (currentScopeID state)
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
  Auxiliary data types
-}

data Location = Location
  { name :: Address,
    idx :: Maybe Address
  }

instance Show Location where
  show (Location nm idx) = T.unpack $ sformat ("Location {name=" % string % ", idx=" % string % "}") (show nm) (show idx)

{-
  Helper functions to manipulate symbol tables.
-}

{- Varaible lookup. -}

lookupLocalVariableDeclFromST :: Name -> SymbolTable -> Maybe (Either Argument FieldDecl)
lookupLocalVariableDeclFromST name st =
  let f = lookupLocalFieldDecl name st
      a = lookupArgument name st
   in (Right <$> f) <|> (Left <$> a)
  where
    lookupLocalFieldDecl name st = Map.lookup name $ variableDecls st
    lookupArgument name st = do
      let SymbolTable {blockType = btpe} = st
      (MethodSig _ _ args) <- case btpe of
        (MethodBlock sig') -> Just sig'
        _ -> Nothing
      find (\(Argument nm _) -> nm == name) args

lookupVariableDecl :: Name -> Semantic (Maybe (Either Argument FieldDecl))
lookupVariableDecl name = do
  st <- getSymbolTable
  return $ st >>= lookup name
  where
    lookup name st' = (lookupLocalVariableDeclFromST name st') <|> (parent st' >>= lookup name)

lookupVariableDecl' :: Name -> Semantic (Either Argument FieldDecl)
lookupVariableDecl' name = do
  v <- lookupVariableDecl name
  case v of
    Nothing -> throwSemanticException ("Varaible " % stext % " not defined") name
    Just v -> return v

lookupLocalVariableFromST :: Name -> SymbolTable -> Maybe Address
lookupLocalVariableFromST name SymbolTable {variableTemporals = vars} =
  Map.lookup name vars >>= listToMaybe

lookupLocalVariable :: Name -> Semantic (Maybe Address)
lookupLocalVariable name = do
  st <- getSymbolTable
  return $ st >>= (lookupLocalVariableFromST name)

lookupLocalVariable' :: Name -> Semantic Address
lookupLocalVariable' name = do
  var <- lookupLocalVariable name
  case var of
    (Just var') -> return var'
    Nothing -> throwSemanticException ("variable " % stext % " not found") name

lookupVariable :: Name -> Semantic (Maybe Address)
lookupVariable name = do
  st <- getSymbolTable
  return $ st >>= lookup name
  where
    lookup name st = (lookupLocalVariableFromST name st) <|> (parent st >>= lookup name)

lookupVariable' :: Name -> Semantic Address
lookupVariable' name = do
  var <- lookupVariable name
  case var of
    (Just var') -> return var'
    Nothing -> throwSemanticException ("variable " % stext % " not found") name

{- Method lookup. -}

lookupLocalMethodFromST :: Name -> SymbolTable -> Maybe (Either ImportDecl MethodSig)
lookupLocalMethodFromST name table =
  let method = do
        methodTable <- methods table
        Map.lookup name methodTable
      import' = do
        importTable <- imports table
        Map.lookup name importTable
   in (Right <$> method) <|> (Left <$> import')

lookupMethod :: Name -> Semantic (Maybe (Either ImportDecl MethodSig))
lookupMethod name = do
  lookup name <$> getSymbolTable'
  where
    lookup name table = (lookupLocalMethodFromST name table) <|> (parent table >>= lookup name)

lookupMethod' :: Name -> Semantic (Either ImportDecl MethodSig)
lookupMethod' name = do
  m <- lookupMethod name
  case m of
    Nothing -> throwSemanticException ("Method " % stext % " not found") name
    Just m' -> return m'

{- Add variables and methods -}

addVariableDef :: FieldDecl -> Semantic Address
addVariableDef def@(FieldDecl nm tpe _) = do
  localST <- getSymbolTable'
  -- Semantic[1]
  when
    (isJust (lookupLocalVariableDeclFromST nm localST))
    (addSemanticError ("duplicate definition for variable " % stext) nm)
  let variableSymbols' = Map.insert (name (def :: FieldDecl)) def (variableDecls localST)
      newST = localST {variableDecls = variableSymbols'}
  -- Semantic[4]
  case def of
    (FieldDecl _ _ (Just sz))
      | sz < 0 ->
        addSemanticError ("Invalid size of array " % stext) nm
    _ -> return ()
  updateSymbolTable newST
  newVariable (Just nm) tpe

genSym :: Maybe Name -> Type -> Semantic Address
genSym nm tpe = do
  id <- gets nextSymbolID
  modify $ \s -> s{nextSymbolID = id + 1}
  case nm of
    Nothing -> do
      return $ Temporal id tpe
    Just nm' -> do
      return $ Variable id nm' tpe

newVariable :: Maybe Name -> Type -> Semantic Address
newVariable nm tpe = do
  st@SymbolTable {variableTemporals = varMap} <- getSymbolTable'
  var <- genSym nm tpe
  case nm of
    (Just nm') -> do
      let newVarMap = Map.alter (\vars -> Just $ var : concat (maybeToList vars)) nm' varMap
      updateSymbolTable st {variableTemporals = newVarMap}
    Nothing -> return ()
  return var

updateVariable :: Address -> Semantic Address
updateVariable (Variable _ name tpe) = newVariable (Just name) tpe
updateVariable _ = throwSemanticException "mutating temporaries or consts!"

addImportDef :: ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  -- Semantic[1]
  let nm = name (def :: ImportDecl)
  when
    (isJust $ Map.lookup (name (def :: ImportDecl)) importTable)
    (addSemanticError ("duplicate import " % stext) nm)
  let importSymbols' = Map.insert (name (def :: ImportDecl)) def importTable
      newST = localST {imports = Just importSymbols'}
  updateSymbolTable newST

addMethodDef :: MethodSig -> [IRInstruction] -> Semantic ()
addMethodDef def instrs = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  -- Semantic[1]
  let nm = name (def :: MethodSig)
  when
    (isJust $ lookupLocalMethodFromST nm localST)
    (addSemanticError ("duplicate definition for method " % stext) nm)
  let methodSymbols' = Map.insert nm def methodTable
      newST = localST {methods = Just methodSymbols'}
  updateSymbolTable newST
  tellInstrs [MethodInstrs def instrs]

{-
  Helper methods to do semantic checks.
-}

-- Semantic[8] and Semantic[9]
-- checkReturnType :: Maybe Name -> Semantic ()
-- checkReturnType Nothing = do
--   (MethodSig method tpe _) <- getMethodSignature'
--   case tpe of
--     Just t -> addSemanticError $ printf "Method %s expects return type of %s!" method (show t)
--     _ -> return ()
-- checkReturnType (Just nm) = do
--   (MethodSig method tpe _) <- getMethodSignature'
--   case tpe of
--     Nothing -> addSemanticError $ printf "Method %s expects no return value!" method
--     t
--       | t /= tpe ->
--         addSemanticError $
--           printf
--             "Method %s expects return type of %v, but got %v instead."
--             method
--             (show tpe)
--             (show tpe')
--     _ -> return ()

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
  unless
    ( (isNegative && (T.drop 1 lit) <= "9223372036854775808")
        || (not isNegative && lit <= "9223372036854775807")
    )
    (throwSemanticException ("Int literal " % stext % " is out of bound") lit)
  case T.decimal lit of
    Right (n, _) -> return n
    Left msg -> throwSemanticException ("cannot parse int literal " % string) msg

checkBoolLiteral :: Text -> Semantic Bool
checkBoolLiteral lit
  | lit == "true" = return True
  | lit == "flase" = return False
  | otherwise = do
    addSemanticError ("error parsing bool literal from string " % stext) lit
    return True

checkCharLiteral :: Text -> Semantic Char
checkCharLiteral lit = do
  when
    (T.length lit > 1 || T.null lit)
    (throwSemanticException ("cannot parse char literal from string " % stext) lit)
  return $ T.head lit

isInsideLoop :: Semantic Bool
isInsideLoop = do
  lookup <$> getSymbolTable'
  where
    lookup SymbolTable {blockType = ForBlock} = True
    lookup SymbolTable {blockType = WhileBlock} = True
    lookup SymbolTable {blockType = IfBlock, parent = Nothing} = False
    lookup SymbolTable {blockType = IfBlock, parent = Just p} = lookup p

--   -- check method "main"
--   -- Semantic[3]
--   globalTable <- getGlobalSymbolTable'
--   let main = do
--         methodSyms <- methodSymbols globalTable
--         Map.lookup mainMethodName methodSyms
--   mainDecl <- checkMainExist main
--   case mainDecl >>= Just . sig of
--     Just (MethodSig _ retType args) -> do
--       checkMainRetType retType
--       checkMainArgsType args
--     Nothing -> return ()
--   return $ IRRoot imports' variables' methods'
--   where
--     checkMainExist main =
--       case main of
--         Nothing -> do
--           addSemanticError "Method \"main\" not found!"
--           return Nothing
--         Just decl -> return $ Just decl
--     checkMainRetType tpe = case tpe of
--       Nothing -> return ()
--       Just tpe ->
--         addSemanticError $
--           printf
--             "Method \"main\" should have return type of void, got %s instead."
--             (show tpe)
--     checkMainArgsType args =
--       unless
--         (null args)
--         (addSemanticError "Method \"main\" should have no argument.")

irgenType :: P.Type -> Type
irgenType P.IntType = IntType
irgenType P.BoolType = BoolType

{-
  Methods to generate ir piece by piece.
-}

irgenRoot :: P.Program -> Semantic ()
irgenRoot (P.Program imports fields methods) = do
  irgenImports imports
  irgenFieldDecls fields
  irgenMethodDecls methods

irgenImports :: [P.WithPos P.ImportDecl] -> Semantic ()
irgenImports [] = return ()
irgenImports ((P.WithPos (P.ImportDecl id) pos) : rest) = do
  updatePosn pos
  let importSymbol = ImportDecl id
  addImportDef importSymbol
  -- TODO: This kind of recursions potentially lead to stack overflows.
  -- For now it should do the job. Will try to fix in the future.
  irgenImports rest

irgenFieldDecls :: [P.WithPos P.FieldDecl] -> Semantic ()
irgenFieldDecls [] = return ()
irgenFieldDecls ((P.WithPos decl pos) : rest) = do
  updatePosn pos
  fields <- sequence $ convertFieldDecl decl
  addVariables fields
  irgenFieldDecls rest
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

irgenMethodDecls :: [P.WithPos P.MethodDecl] -> Semantic ()
irgenMethodDecls [] = return ()
irgenMethodDecls ((P.WithPos decl pos) : rest) = do
  updatePosn pos
  (sig, instrs) <- convertMethodDecl decl
  -- Semantic[8] and Semantic[9]
  -- checkMethod method
  addMethodDef sig instrs
  irgenMethodDecls rest
  where
    convertMethodDecl (P.MethodDecl id returnType arguments block) = do
      let sig = MethodSig id (irgenType <$> returnType) args
      irgenMethodBody sig block
      instrs <- offloadInstructions
      return (sig, instrs)
      where
        args = arguments <&> \(P.WithPos (P.Argument id tpe) _) -> Argument id (irgenType tpe)

irgenMethodBody :: MethodSig -> P.Block -> Semantic ()
irgenMethodBody sig (P.Block fieldDecls statements) = do
  enterScope $ MethodBlock sig
  irgenFieldDecls fieldDecls
  irgenStatements statements
  exitScope

irgenStatements :: [P.WithPos P.Statement] -> Semantic ()
irgenStatements [] = return ()
irgenStatements ((P.WithPos s pos) : xs) = do
  updatePosn pos
  irgenStmt s
  irgenStatements xs

irgenLocationCheck :: P.Location -> Semantic ()
irgenLocationCheck (P.ScalarLocation id) = do
  def <- lookupVariableDecl' id
  let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
  when
    (isJust sz)
    (throwSemanticException ("Cannot assign to vector variable " % stext % "") id)
irgenLocationCheck (P.VectorLocation id expr) = do
  (Temporal _ indexTpe) <- irgenExpr expr
  def <- lookupVariableDecl' id
  --let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
  let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
  -- Semantic[12]
  when (indexTpe /= IntType) (addSemanticError "Index must be of int type!")
  when
    (isNothing sz)
    (addSemanticError ("Cannot access index of scalar variable " % stext % ".") id)

irgenReadScalarFromLocation :: P.Location -> Semantic (Address, Maybe Address)
irgenReadScalarFromLocation loc@(P.ScalarLocation id) = do
  --irgenLocationCheck loc
  var <- lookupVariable' id
  return (var, Nothing)
irgenReadScalarFromLocation loc@(P.VectorLocation id expr) = do
  --irgenLocationCheck loc
  indexVar <- irgenExpr expr
  array@(Variable _ _ tpe) <- lookupVariable' id
  var <- newVariable Nothing tpe
  addInstructions [ArrayToScalarCopy var array indexVar]
  return (var, Just indexVar)

irgenAssign :: P.Location -> P.AssignExpr -> Semantic ()
irgenAssign loc (P.AssignExpr op expr) = do
  let op' = parseAssignOp op
  expr' <- irgenExpr expr
  (var@(Variable _ _ tpe), index) <- case loc of
    (P.ScalarLocation id) -> do
      var <- lookupVariable' id
      return (var, Nothing)
    (P.VectorLocation id index) -> do
      index' <- irgenExpr index
      var <- lookupVariable' id
      return (var, Just index')
  -- Semantic[20]
  -- when
  --   ((op' == IncAssign || op' == DecAssign) && (tpe /= IntType))
  --   (addSemanticError "Inc or dec assign only works with int type!")
  let arithOp = case op' of
        IncAssign -> Just Plus
        DecAssign -> Just Minus
        _ -> Nothing
  src <- case arithOp of
    Just op -> do
      (var, sz) <- irgenReadScalarFromLocation loc
      arithResult <- newVariable Nothing IntType
      addInstructions [Arithmetic arithResult op var expr']
      return arithResult
    Nothing -> return expr'
  case index of
    Nothing -> do
      new <- updateVariable var
      addInstructions [ScalarCopy new src]
    (Just idx) -> do
      addInstructions [ScalarToArrayCopy var src idx]
irgenAssign loc (P.IncrementExpr op) = do
  let id = P.locationId loc
  original <- lookupVariable' id
  (var, idx) <- irgenReadScalarFromLocation loc
  let tpe = typeOf var
  let op' = parseAssignOp op
  let arithOp = case op' of
        PlusPlus -> Plus
        MinusMinus -> Minus
  when (tpe /= IntType) (addSemanticError "Inc or dec operator only works on int type!")
  case idx of
    Nothing -> do
      newVar <- updateVariable original
      addInstructions [Arithmetic newVar arithOp var (Constant $ IntLiteral 1)]
    Just idx' -> do
      newArray <- updateVariable original
      temp <- newVariable Nothing tpe
      addInstructions
        [ Arithmetic temp arithOp var (Constant $ IntLiteral 1),
          ScalarToArrayCopy newArray temp idx'
        ]

irgenStmt :: P.Statement -> Semantic ()
irgenStmt (P.AssignStatement loc expr) = do
  assign <- irgenAssign loc expr
  return ()

irgenExpr :: P.WithPos P.Expr -> Semantic Address
irgenExpr (P.WithPos (P.LocationExpr loc) posn) = do
  updatePosn posn
  (var, _) <- irgenReadScalarFromLocation loc
  return var
irgenExpr (P.WithPos (P.IntLiteralExpr i) posn) = do
  updatePosn posn
  literalVal <- checkInt64Literal i
  return $ Constant $ IntLiteral literalVal
irgenExpr (P.WithPos (P.CharLiteralExpr c) posn) = do
  updatePosn posn
  charVal <- checkCharLiteral c
  return $ Constant $ CharLiteral charVal
irgenExpr (P.WithPos (P.BoolLiteralExpr c) posn) = do
  updatePosn posn
  boolVal <- checkBoolLiteral c
  return $ Constant $ BoolLiteral boolVal

-- irgenStmt (P.MethodCallStatement method) = do
--   method' <- irgenMethod method
--   return $ MethodCallStmt method'
-- irgenStmt (P.IfStatement expr block) = do
--   ifBlock <- irgenBlock IfBlock block
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe))
--   return $ IfStmt expr' ifBlock Nothing
-- irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
--   ifBlock' <- irgenBlock IfBlock ifBlock
--   elseBlock' <- irgenBlock IfBlock elseBlock
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe))
--   return $ IfStmt expr' ifBlock' (Just elseBlock')
-- irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
--   block' <- irgenBlock ForBlock block
--   counterExpr' <- irgenExpr counterExpr
--   predExpr'@(WithType _ tpe) <- irgenExpr predExpr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of for statment must have type bool, but got %s instead!" (show tpe))
--   assign <- irgenAssign loc expr
--   return $ ForStmt counter counterExpr' predExpr' assign block'
-- irgenStmt (P.WhileStatement expr block) = do
--   block' <- irgenBlock WhileBlock block
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of while statment must have type bool, but got %s instead!" (show tpe))
--   return $ WhileStmt expr' block'
-- irgenStmt (P.ReturnExprStatement expr) = do
--   expr' <- irgenExpr expr
--   -- Semantic[8] and Semantic[9]
--   checkReturnType $ Just expr'
--   return $ ReturnStmt $ Just expr'
-- irgenStmt P.ReturnVoidStatement = do
--   -- Semantic[8] and Semantic[9]
--   checkReturnType Nothing
--   return $ ReturnStmt Nothing
-- irgenStmt P.BreakStatement = do
--   -- Semantic[21]
--   inLoop <- isInsideLoop
--   unless
--     inLoop
--     (addSemanticError "Found break statement outside for or while block!")
--   return BreakStmt
-- irgenStmt P.ContinueStatement = do
--   -- Semantic[21]
--   inLoop <- isInsideLoop
--   unless
--     inLoop
--     (addSemanticError "Found continue statement outside for or while block!")
--   return ContinueStmt

-- irgenLocation :: P.Location -> Semantic Location
-- irgenLocation (P.ScalarLocation id) = do
--   -- Semantic[10] (checked in lookupVariable')
--   def <- lookupVariable' id
--   let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
--   let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
--   -- Semantic[12]
--   case sz of
--     Nothing -> return $ WithType (Location id Nothing def) tpe
--     Just _ -> return $ WithType (Location id Nothing def) (ArrayType tpe)
-- irgenLocation (P.VectorLocation id expr) = do
--   (WithType expr' indexTpe) <- irgenExpr expr
--   -- Semantic[10] (checked in lookupVariable')
--   def <- lookupVariable' id
--   let tpe = either (\(Argument _ tpe') -> tpe') (\(FieldDecl _ tpe' _) -> tpe') def
--   let sz = either (const Nothing) (\(FieldDecl _ _ sz') -> sz') def
--   -- Semantic[12]
--   when (indexTpe /= IntType) (addSemanticError "Index must be of int type!")
--   case sz of
--     Nothing -> do
--       addSemanticError $
--         printf "Cannot access index of scalar variable %s." id
--       return $ WithType (Location id Nothing def) tpe
--     Just _ -> return $ WithType (Location id (Just expr') def) tpe

-- irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
-- irgenAssign loc (P.AssignExpr op expr) = do
--   loc'@(WithType _ tpe) <- irgenLocation loc
--   expr'@(WithType _ tpe') <- irgenExpr expr
--   -- Semantic[19]
--   when
--     (tpe /= tpe')
--     (addSemanticError $ printf "Assign statement has different types: %s and %s" (show tpe) (show tpe'))
--   let op' = parseAssignOp op
--   -- Semantic[20]
--   when
--     ((op' == IncAssign || op' == DecAssign) && (tpe /= IntType))
--     (addSemanticError $ printf "Inc or dec assign only works with int type!")
--   return $ Assignment loc' op' (Just expr')
-- irgenAssign loc (P.IncrementExpr op) = do
--   loc'@(WithType _ tpe) <- irgenLocation loc
--   let op' = parseAssignOp op
--   -- Semantic[20]
--   when (tpe /= IntType) (addSemanticError $ printf "Inc or dec operator only works on int type!")
--   return $ Assignment loc' op' Nothing

-- irgenStatements :: [P.WithPos P.Statement] -> Semantic [Statement]
-- irgenStatements [] = return []
-- irgenStatements ((P.WithPos s pos) : xs) = do
--   updatePosn pos
--   s' <- irgenStmt s
--   xs' <- irgenStatements xs
--   return (s' : xs')

-- irgenMethod :: P.MethodCall -> Semantic MethodCall
-- irgenMethod (P.MethodCall method args') = do
--   -- Semantic[2] and Semantic[11]
--   decl' <- lookupMethod method
--   argsWithType <- sequenceA $ irgenImportArg <$> args'
--   case decl' of
--     Nothing -> do
--       currentMethod <- getMethodSignature
--       case currentMethod of
--         -- Recursive method calling itself
--         (Just (MethodSig name _ formal)) | name == method -> do
--           checkCallingSemantics formal argsWithType
--           return $ MethodCall method argsWithType
--         _ -> throwSemanticException $ printf "method %s not declared!" method
--     Just decl -> case decl of
--       Left _ -> return $ MethodCall method argsWithType
--       Right m -> do
--         let formal = args (sig m :: MethodSig)
--         -- Semantic[5] and Semantic[7]
--         checkCallingSemantics formal argsWithType
--         return $ MethodCall method argsWithType
--   where
--     matchPred (Argument _ tpe, WithType _ tpe') = tpe == tpe'
--     argName (Argument name _, _) = name
--     checkArgNum formal args =
--       unless
--         (length formal == length args)
--         ( addSemanticError $
--             printf
--               "Calling %s with wrong number of args. Required: %d, supplied: %d."
--               method
--               (length formal)
--               (length args)
--         )
--     checkArgType formal args =
--       let mismatch = map argName $ filter (not . matchPred) $ zip formal args
--        in unless
--             (null mismatch)
--             ( addSemanticError $
--                 printf
--                   "Calling %s with wrong type of args: %s"
--                   method
--                   (show mismatch)
--             )
--     arrayOrStringTypePred (WithType _ tpe) = case tpe of
--       ArrayType _ -> True
--       StringType -> True
--       _ -> False
--     checkForArrayArg args =
--       let arrayArgs = map ele $ filter arrayOrStringTypePred args
--        in unless
--             (null arrayArgs)
--             ( addSemanticError $
--                 printf
--                   "Argument of array or string type can not be used for method %s"
--                   method
--             )
--     checkCallingSemantics formal args = do
--       checkArgNum formal args
--       checkArgType formal args
--       checkForArrayArg args

-- irgenStmt :: P.Statement -> Semantic Statement
-- irgenStmt (P.AssignStatement loc expr) = do
--   assign <- irgenAssign loc expr
--   return $ AssignStmt assign
-- irgenStmt (P.MethodCallStatement method) = do
--   method' <- irgenMethod method
--   return $ MethodCallStmt method'
-- irgenStmt (P.IfStatement expr block) = do
--   ifBlock <- irgenBlock IfBlock block
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe))
--   return $ IfStmt expr' ifBlock Nothing
-- irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
--   ifBlock' <- irgenBlock IfBlock ifBlock
--   elseBlock' <- irgenBlock IfBlock elseBlock
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of if statment must have type bool, but got %s instead!" (show tpe))
--   return $ IfStmt expr' ifBlock' (Just elseBlock')
-- irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
--   block' <- irgenBlock ForBlock block
--   counterExpr' <- irgenExpr counterExpr
--   predExpr'@(WithType _ tpe) <- irgenExpr predExpr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of for statment must have type bool, but got %s instead!" (show tpe))
--   assign <- irgenAssign loc expr
--   return $ ForStmt counter counterExpr' predExpr' assign block'
-- irgenStmt (P.WhileStatement expr block) = do
--   block' <- irgenBlock WhileBlock block
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   -- Semantic[14]
--   when
--     (tpe /= BoolType)
--     (addSemanticError $ printf "The pred of while statment must have type bool, but got %s instead!" (show tpe))
--   return $ WhileStmt expr' block'
-- irgenStmt (P.ReturnExprStatement expr) = do
--   expr' <- irgenExpr expr
--   -- Semantic[8] and Semantic[9]
--   checkReturnType $ Just expr'
--   return $ ReturnStmt $ Just expr'
-- irgenStmt P.ReturnVoidStatement = do
--   -- Semantic[8] and Semantic[9]
--   checkReturnType Nothing
--   return $ ReturnStmt Nothing
-- irgenStmt P.BreakStatement = do
--   -- Semantic[21]
--   inLoop <- isInsideLoop
--   unless
--     inLoop
--     (addSemanticError "Found break statement outside for or while block!")
--   return BreakStmt
-- irgenStmt P.ContinueStatement = do
--   -- Semantic[21]
--   inLoop <- isInsideLoop
--   unless
--     inLoop
--     (addSemanticError "Found continue statement outside for or while block!")
--   return ContinueStmt

-- {- generate expressions, also do type inference -}
-- irgenExpr :: P.WithPos P.Expr -> Semantic (WithType Expr)
-- irgenExpr (P.WithPos (P.LocationExpr loc) pos) = do
--   updatePosn pos
--   (WithType loc' tpe) <- irgenLocation loc
--   return $ WithType (LocationExpr loc') tpe
-- irgenExpr (P.WithPos (P.MethodCallExpr method@(P.MethodCall name _)) pos) = do
--   updatePosn pos
--   method' <- irgenMethod method
--   m <- lookupMethod' name
--   case m of
--     -- treat import methods as always return int
--     Left _ -> return $ WithType (MethodCallExpr method') IntType
--     Right (MethodDecl (MethodSig _ tpe _) _) -> do
--       case tpe of
--         -- Semantic[6]
--         Nothing ->
--           throwSemanticException $
--             printf "Method %s cannot be used in expressions as it returns nothing!" name
--         Just tpe' -> return $ WithType (MethodCallExpr method') tpe'
-- irgenExpr (P.WithPos (P.IntLiteralExpr i) pos) = do
--   updatePosn pos
--   literalVal <- checkInt64Literal $ i
--   return $ WithType (IntLiteralExpr literalVal) IntType
-- irgenExpr (P.WithPos (P.BoolLiteralExpr b) pos) = do
--   updatePosn pos
--   lit <- checkBoolLiteral b
--   return $ WithType (BoolLiteralExpr lit) BoolType
-- irgenExpr (P.WithPos (P.CharLiteralExpr c) pos) = do
--   updatePosn pos
--   lit <- checkCharLiteral c
--   return $ WithType (CharLiteralExpr lit) IntType
-- irgenExpr (P.WithPos (P.LenExpr id) pos) = do
--   updatePosn pos
--   def <- lookupVariable' id
--   -- Semantic[13]
--   case def of
--     Left (Argument nm _) -> addSemanticError $ printf "len cannot operate on argument %s!" nm
--     Right (FieldDecl nm _ sz) ->
--       when
--         (isNothing sz)
--         (addSemanticError $ printf "len cannot operate on scalar variable %s!" nm)
--   return $ WithType (LengthExpr id) IntType
-- irgenExpr (P.WithPos (P.ArithOpExpr op l r) pos) = do
--   updatePosn pos
--   -- Semantic[16]
--   l'@(WithType _ ltp) <- irgenExpr l
--   r'@(WithType _ rtp) <- irgenExpr r
--   when
--     (ltp /= IntType || rtp /= IntType)
--     (addSemanticError "There can only be integer values in arithmetic expressions.")
--   return $ WithType (ArithOpExpr (parseArithOp op) l' r') IntType
-- irgenExpr (P.WithPos (P.RelOpExpr op l r) pos) = do
--   updatePosn pos
--   -- Semantic[16]
--   l'@(WithType _ ltp) <- irgenExpr l
--   r'@(WithType _ rtp) <- irgenExpr r
--   when
--     (ltp /= IntType || rtp /= IntType)
--     (addSemanticError "There can only be integer values in relational expressions.")
--   return $ WithType (RelOpExpr (parseRelOp op) l' r') IntType
-- irgenExpr (P.WithPos (P.EqOpExpr op l r) pos) = do
--   updatePosn pos
--   -- Semantic[17]
--   l'@(WithType _ ltp) <- irgenExpr l
--   r'@(WithType _ rtp) <- irgenExpr r
--   when
--     ((ltp, rtp) /= (IntType, IntType) && (ltp, rtp) /= (BoolType, BoolType))
--     (addSemanticError "Can only check equality of expressions with the SAME type!")
--   return $ WithType (EqOpExpr (parseEqOp op) l' r') BoolType
-- irgenExpr (P.WithPos (P.CondOpExpr op l r) pos) = do
--   updatePosn pos
--   -- Semantic[18]
--   l'@(WithType _ ltp) <- irgenExpr l
--   r'@(WithType _ rtp) <- irgenExpr r
--   when
--     (ltp /= BoolType || rtp /= BoolType)
--     (addSemanticError "Conditional ops only accept booleans!")
--   return $ WithType (CondOpExpr (parseCondOp op) l' r') BoolType
-- irgenExpr (P.WithPos (P.NegativeExpr expr) pos) = do
--   updatePosn pos
--   -- Semantic[16]
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   when
--     (tpe /= IntType)
--     (addSemanticError "Operator \"-\" only accepts integers!")
--   return $ WithType (NegOpExpr Neg expr') IntType
-- irgenExpr (P.WithPos (P.NegateExpr expr) pos) = do
--   updatePosn pos
--   -- Semantic[18]
--   expr'@(WithType _ tpe) <- irgenExpr expr
--   when
--     (tpe /= BoolType)
--     (addSemanticError "Operator \"!\" only accepts integers!")
--   return $ WithType (NotOpExpr Not expr') BoolType
-- irgenExpr (P.WithPos (P.ChoiceExpr pred l r) pos) = do
--   updatePosn pos
--   pred'@(WithType _ ptp) <- irgenExpr pred
--   l'@(WithType _ ltp) <- irgenExpr l
--   r'@(WithType _ rtp) <- irgenExpr r
--   -- Semantic[15]
--   when
--     (ptp /= BoolType)
--     (addSemanticError "Predicate of choice operator must be a boolean!")
--   when
--     (ltp /= rtp)
--     (addSemanticError "Alternatives of choice op should have same type!")
--   return $ WithType (ChoiceOpExpr Choice pred' l' r') ltp
-- irgenExpr (P.WithPos (P.ParenExpr expr) pos) = do
--   updatePosn pos
--   irgenExpr expr

-- irgenImportArg :: P.ImportArg -> Semantic (WithType Expr)
-- irgenImportArg (P.ExprImportArg expr) = irgenExpr expr
-- irgenImportArg (P.StringImportArg (P.WithPos arg pos)) = do
--   updatePosn pos
--   return $ WithType (StringLiteralExpr arg) StringType
