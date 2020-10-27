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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module IR ( generate
          , Location(..), Assignment(..), MethodCall(..)
          , IRRoot(..), ImportDecl(..), FieldDecl(..), MethodDecl(..)
          , Statement(..), Expr(..), Block(..)
          , Name
          , BinaryOp
          , UnaryOp
          , TernaryOp
          , AssignOp
          , Type
          , SemanticState
          , SemanticException
          , SemanticError
          , runSemanticAnalysis
          ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.Foldable
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Text.Printf               (printf)

import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as B

import qualified Parser                    as P

type Name = ByteString
type ScopeID = Int

-- operators
data BinaryOp =
  Plus
  | Minus
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | Equal
  | NotEqual
  | OR
  | AND
  deriving (Show, Eq)

data UnaryOp =
  Negate
  | Negative
  deriving (Show, Eq)

data TernaryOp =
  Choice
  deriving (Show, Eq)

data AssignOp =
  EqlAssign
  | IncAssign
  | DecAssign
  | PlusPlus
  | MinusMinus
  deriving (Show, Eq)

data Type =
  IntType
  | BoolType
  deriving (Show, Eq)

parseBinaryOp :: ByteString -> BinaryOp
parseBinaryOp op = case op of
  "+"  -> Plus
  "-"  -> Minus
  "<"  -> LessThan
  ">"  -> GreaterThan
  "<=" -> LessEqual
  ">=" -> GreaterEqual
  "==" -> Equal
  "!=" -> NotEqual
  "||" -> OR
  "&&" -> AND

parseUnaryOp :: ByteString -> UnaryOp
parseUnaryOp op = case op of
  "-" -> Negative
  "!" -> Negate

parseAssignOp :: ByteString -> AssignOp
parseAssignOp s = case s of
  "+=" -> IncAssign
  "-=" -> DecAssign
  "="  -> EqlAssign
  "++" -> PlusPlus
  "--" -> MinusMinus

-- auxiliary data types
data Location = Location { name :: Name
                         , idx  :: Maybe Expr
                         } deriving Show

data Assignment = Assignment { location :: Location
                             , op       :: AssignOp
                             , expr     :: Maybe Expr
                             } deriving Show

data MethodCall = MethodCall { name :: Name
                             , args :: [Expr]
                             } deriving Show

-- ir nodes
data IRRoot = IRRoot { imports :: [ImportDecl]
                     , vars    :: [FieldDecl]
                     , methods :: [MethodDecl]
                     } deriving Show

data ImportDecl = ImportDecl { name :: Name }
  deriving Show

data FieldDecl = FieldDecl { name :: Name
                           , tpe  :: Type
                           , size :: Maybe Int
                           } deriving Show

data MethodDecl = MethodDecl { name  :: Name
                             , tpe   :: (Maybe Type)
                             , args  :: [(Name, Type)]
                             , block :: Block
                             } deriving Show

data Statement = AssignStmt { assign :: Assignment }
               | IfStmt { pred :: Expr, ifBlock :: Block, elseBlock :: Maybe Block}
               | ForStmt { counter     :: Name
                         , initCounter :: Expr
                         , pred        :: Expr
                         , update      :: Assignment
                         , block       :: Block}
               | WhileStmt { pred :: Expr, block :: Block}
               | ReturnStmt { expr :: (Maybe Expr) }
               | MethodCallStmt { methodCall :: MethodCall }
               | BreakStmt
               | ContinueStmt
               deriving Show

data Expr = LocationExpr { location :: Location }
          | MethodCallExpr { methodCall :: MethodCall }
          | ExternCallExpr { name :: Name, args :: [Expr] }
          | IntLiteralExpr { intVal :: Int }
          | BoolLiteralExpr { boolVal :: Bool }
          | CharLiteralExpr { charVal :: Char }
          | StringLiteralExpr { strVal :: ByteString }
          | BinaryOpExpr { binaryOp :: BinaryOp, lhs :: Expr, rhs :: Expr }
          | UnaryOpExpr { uanryOp :: UnaryOp, expr :: Expr }
          | TernaryOpExpr { tenaryOp :: TernaryOp, expr1 :: Expr, expr2 :: Expr, expr3 :: Expr }
          | LengthExpr { name :: Name }
          deriving Show

data Block = Block { vars    :: [FieldDecl]
                   , stats   :: [Statement]
                   , blockID :: ScopeID
                   } deriving Show

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
data SymbolTable = SymbolTable { scopeID   :: ScopeID
                               , parent    :: Maybe SymbolTable
                               , importSymbols   :: Maybe (Map IR.Name IR.ImportDecl)
                               , variableSymbols :: Map IR.Name IR.FieldDecl
                               , methodSymbols   :: Maybe (Map IR.Name IR.MethodDecl)
                               } deriving Show
data SemanticState = SemanticState
  { nextScopeID    :: ScopeID
  , currentScopeID :: ScopeID
  , symbolTables   :: Map ScopeID SymbolTable
  } deriving Show

-- Monad used for semantic analysis
-- Symbol tables are built for every scope, and stored in SemanticState.
-- Semantic errors encountered are recorded by the writer monad (WriterT [SemanticError]).
-- If a serious problem happened such that the analysis has to be aborted, a SemanticException
-- is thrown.
newtype Semantic a = Semantic { runSemantic :: ExceptT SemanticException (WriterT [SemanticError] (State SemanticState))  a }
  deriving (Functor, Applicative, Monad, MonadError SemanticException, MonadWriter [SemanticError], MonadState SemanticState)

runSemanticAnalysis :: Semantic a -> Either String (a, [SemanticError], SemanticState)
runSemanticAnalysis s =
  let ((except, errors), state) = (runState $ runWriterT $ runExceptT $ runSemantic s) initialSemanticState
  in
    case except of
      Left (SemanticException msg) -> Left $ B.toString msg
      Right a                      -> Right (a, errors, state)

throwSemanticException :: String -> Semantic a
throwSemanticException msg = throwError $ SemanticException $ B.fromString msg

addSemanticError :: String -> Semantic ()
addSemanticError msg = tell $ [ SemanticError (B.fromString msg) ]

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
    Nothing    -> throwSemanticException $
      printf "No symble table found for current scope %d" $ scopeID
    Just table -> return table

getLocalVariables' :: Semantic (Map IR.Name IR.FieldDecl)
getLocalVariables' = do
  localST <- getSymbolTable'
  return $ variableSymbols localST

getLocalImports' :: Semantic (Map IR.Name IR.ImportDecl)
getLocalImports' = do
  localST <- getSymbolTable'
  case importSymbols localST of
    Nothing -> throwSemanticException $ printf "No import table for scope %d" $ scopeID localST
    Just t -> return t

getLocalMethods' :: Semantic (Map IR.Name IR.MethodDecl)
getLocalMethods' = do
  localST <- getSymbolTable'
  case methodSymbols localST of
    Nothing -> throwSemanticException $ printf "No method table for scope %d" $ scopeID localST
    Just t  -> return t

updateSymbolTable :: SymbolTable -> Semantic ()
updateSymbolTable t = do
  state <- get
  -- ensure the symbol table is present, otherwise throw an exception
  getSymbolTable'
  put $ state { symbolTables = Map.insert (currentScopeID state) t (symbolTables state) }

-- lookup symbol in local scope.
-- this is useful to find conflicting symbol names.
lookupLocalVariable :: IR.Name -> Semantic (Maybe IR.FieldDecl)
lookupLocalVariable name = do
  table <- getSymbolTable
  return $ table >>= (Map.lookup name . variableSymbols)

lookupVariable :: IR.Name -> Semantic (Maybe IR.FieldDecl)
lookupVariable name = do
  local <- getSymbolTable
  return $ local >>= lookup name
  where lookup name table = case Map.lookup name (variableSymbols table) of
          -- found in local symbol table
          Just s -> Just s
          -- otherwise search in parent table
          Nothing -> case (parent table) of
                       Nothing -> Nothing
                       Just p  -> lookup name p

lookupLocalMethod' :: IR.Name -> SymbolTable -> Maybe (Either IR.ImportDecl IR.MethodDecl)
lookupLocalMethod' name table =
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

lookupLocalMethod :: IR.Name -> Semantic (Maybe (Either IR.ImportDecl IR.MethodDecl))
lookupLocalMethod name = do
  table <- getSymbolTable
  return $ table >>= lookupLocalMethod' name

lookupMethod:: IR.Name -> Semantic (Maybe (Either IR.ImportDecl IR.MethodDecl))
lookupMethod name = do
  local <- getSymbolTable'
  return $ lookup name local
  where lookup name table = case lookupLocalMethod' name table of
          -- found in local symbol table
          Just s -> Just s
          -- otherwise search in parent table
          Nothing -> case (parent table) of
                       Nothing -> Nothing
                       Just p  -> lookup name p

enterScope :: IR.Block -> Semantic ()
enterScope block = do
  state <- get
  parentST <- getSymbolTable
  let currentID = currentScopeID state
      nextID = nextScopeID state
      localST = SymbolTable { scopeID = nextID
                            , parent = parentST
                            , variableSymbols = Map.empty
                            , importSymbols = Nothing
                            , methodSymbols = Nothing
                            }
  put $ state { nextScopeID = nextID + 1
              , currentScopeID = nextID
              , symbolTables = Map.insert nextID localST $ symbolTables state
              }

exitScope :: Semantic ()
exitScope = do
  state <- get
  localST <- getSymbolTable
  case localST of
    Nothing ->
      throwSemanticException
        $ printf "No symbol table is associated with scope(%d)!" $ currentScopeID state
    Just table ->
      case parent table of
        Nothing ->
          throwSemanticException "Cannot exit root scope!"
        Just p ->
          put $ state { currentScopeID = scopeID p }

----------------------------------------------------------------------
-- Convert the parser tree into an IR tree
-- Generate symbol tables at the same time.
-- Also detects semantic errors.
----------------------------------------------------------------------

{-
Semantic rules to be checked:
1. Identifier duplication.
   We check this when adding symbol definitions.
2. Identifier should be declared before used.
   Check when identifier get used. This only happends in expressions
   and method calls.
-}

addVariableDef :: IR.FieldDecl -> Semantic ()
addVariableDef def = do
  localST <- getSymbolTable'
  -- check for duplications
  let nm = IR.name (def :: IR.FieldDecl)
  case Map.lookup (IR.name (def :: IR.FieldDecl)) (variableSymbols localST) of
    Just _ -> addSemanticError $ printf "duplicate definition for variable %s" $ B.toString nm
    _      -> return ()
  let variableSymbols' = Map.insert (IR.name (def :: IR.FieldDecl)) def (variableSymbols localST)
      newST = localST { variableSymbols = variableSymbols' }
  -- ensure declared array size > 0
  case IR.size def of
    Just s -> if s < 0 then addSemanticError (printf "Invalid size of array %s" $ B.toString nm) else tell []
    _      -> return ()
  updateSymbolTable newST

addImportDef :: IR.ImportDecl -> Semantic ()
addImportDef def = do
  localST <- getSymbolTable'
  importTable <- getLocalImports'
  let nm = IR.name (def :: IR.ImportDecl)
  case Map.lookup (IR.name (def :: IR.ImportDecl)) importTable of
    Just _ -> addSemanticError $ printf "duplicate import %s" $ B.toString nm
    _      -> return ()
  let importSymbols' = Map.insert (IR.name (def :: IR.ImportDecl)) def importTable
      newST = localST { importSymbols = Just importSymbols' }
  updateSymbolTable newST

addMethodDef :: IR.MethodDecl -> Semantic ()
addMethodDef def = do
  localST <- getSymbolTable'
  methodTable <- getLocalMethods'
  let nm = IR.name (def :: IR.MethodDecl)
  case Map.lookup (IR.name (def :: IR.MethodDecl)) methodTable of
    Just _ -> addSemanticError $ printf "duplicate definition for method %s" $ B.toString nm
  let methodSymbols' = Map.insert (IR.name (def :: IR.MethodDecl)) def methodTable
      newST = localST { methodSymbols = Just methodSymbols' }
  updateSymbolTable newST

initialSemanticState :: SemanticState
initialSemanticState =
  let globalST = SymbolTable { scopeID = 0
                             , parent = Nothing
                             , importSymbols = Just Map.empty
                             , variableSymbols = Map.empty
                             , methodSymbols = Just Map.empty
                             }
  in SemanticState { nextScopeID = 1
                   , currentScopeID = 0
                   , symbolTables = Map.fromList [(0, globalST)]
                   }

generate :: P.Program -> Semantic IRRoot
generate (P.Program imports fields methods) = do
  imports' <- irgenImports imports
  variables' <- irgenFieldDecls fields
  methods' <- irgenMethodDecls methods
  return $ IRRoot imports' variables' methods'

irgenType :: P.Type -> Type
irgenType P.IntType  = IntType
irgenType P.BoolType = BoolType

irgenImports :: [P.ImportDecl] -> Semantic [ImportDecl]
irgenImports []          = return []
irgenImports ((P.ImportDecl id):rest) = do
  let importSymbol = ImportDecl id
  addImportDef importSymbol
  -- TODO: This kind of recursions will potentially lead to stack overflows.
  -- For now it should do the job. Will try to fix in the future.
  rest' <- irgenImports rest
  return $ (importSymbol : rest')

irgenFieldDecls :: [P.FieldDecl] -> Semantic [FieldDecl]
irgenFieldDecls []          = return []
irgenFieldDecls (decl:rest) = do
  let fields = convertFieldDecl decl
  vars <- addVariables fields
  rest' <- irgenFieldDecls rest
  return (vars ++ rest')
  where
    convertFieldDecl (P.FieldDecl tpe elems) =
        flip fmap elems $ \case
            (P.ScalarField id)
                -> FieldDecl id (irgenType tpe) Nothing
            (P.VectorField id size)
                -> FieldDecl id (irgenType tpe) (Just sz)
                where sz = read $ B.toString size
    addVariables [] = return []
    addVariables (v:vs) = do
        addVariableDef v
        vs' <- addVariables vs
        return (v:vs')

irgenMethodDecls :: [P.MethodDecl] -> Semantic [MethodDecl]
irgenMethodDecls [] = return []
irgenMethodDecls (decl:rest) = do
  method <- convertMethodDecl decl
  rest' <- irgenMethodDecls rest
  return (method:rest')
  where
    convertMethodDecl (P.MethodDecl id returnType arguments block) = do
      block <- irgenBlock block
      return $ MethodDecl id (irgenType <$> returnType) args block
      where
          args = map (\(P.Argument id tpe) -> (id, irgenType tpe)) arguments


irgenMethodDecl :: P.MethodDecl -> Semantic MethodDecl
irgenMethodDecl (P.MethodDecl id returnType arguments block) = do
  block <- irgenBlock block
  return $ MethodDecl id (irgenType <$> returnType) args block
  where
    args = map (\(P.Argument id tpe) -> (id, irgenType tpe)) arguments

irgenBlock :: P.Block -> Semantic Block
irgenBlock (P.Block fieldDecls statements) = do
  state <- get
  let nextID = nextScopeID state
  fields <- irgenFieldDecls fieldDecls
  stmts <- irgenStatements statements
  let block = Block fields stmts nextID
  enterScope block
  return block

irgenLocation :: P.Location -> Semantic Location
irgenLocation (P.ScalarLocation id)      = do
  id' <- lookupVariable id
  return $ Location id Nothing
irgenLocation (P.VectorLocation id expr) = do
  expr' <- irgenExpr expr
  return $ Location id (Just expr')

irgenAssign :: P.Location -> P.AssignExpr -> Semantic Assignment
irgenAssign loc (P.AssignExpr op expr) = do
  loc' <- irgenLocation loc
  expr' <- irgenExpr expr
  let op' = parseAssignOp op
  return $ Assignment loc' op' (Just expr')
irgenAssign loc (P.IncrementExpr op) = do
  loc' <- irgenLocation loc
  let op' = parseAssignOp op
  return $ Assignment loc' op' Nothing

irgenStatements :: [P.Statement] -> Semantic [Statement]
irgenStatements [] = return []
irgenStatements (s:xs) = do
  s' <- irgenStmt s
  xs' <- irgenStatements xs
  return (s':xs')

irgenMethod :: P.MethodCall -> Semantic MethodCall
irgenMethod (P.MethodCall method args) = do
  decl' <- lookupVariable method
  args' <- sequenceA $ irgenImportArg <$> args
  case decl' of
    Nothing -> throwSemanticException $ printf "method %s not declared!" (B.toString method)
    _ -> return $ MethodCall method args'

irgenStmt :: P.Statement -> Semantic Statement
irgenStmt (P.AssignStatement loc expr) = do
  assign <- irgenAssign loc expr
  return $ AssignStmt assign
irgenStmt (P.MethodCallStatement method) = do
  method' <- irgenMethod method
  return $ MethodCallStmt method'
irgenStmt (P.IfStatement expr block) = do
  ifBlock <- irgenBlock block
  expr' <- irgenExpr expr
  return $ IfStmt expr' ifBlock Nothing
irgenStmt (P.IfElseStatement expr ifBlock elseBlock) = do
  ifBlock' <- irgenBlock ifBlock
  elseBlock' <- irgenBlock elseBlock
  expr' <- irgenExpr expr
  return $ IfStmt expr' ifBlock' (Just $ elseBlock')
irgenStmt (P.ForStatement counter counterExpr predExpr (P.CounterUpdate loc expr) block) = do
  block' <- irgenBlock block
  counterExpr' <- irgenExpr counterExpr
  predExpr' <- irgenExpr predExpr
  assign <- irgenAssign loc expr
  return $ ForStmt counter counterExpr' predExpr' assign block'
irgenStmt (P.WhileStatement expr block) = do
  block' <- irgenBlock block
  expr' <- irgenExpr expr
  return $ WhileStmt expr' block'
irgenStmt (P.ReturnExprStatement expr) = do
  expr' <- irgenExpr expr
  return $ ReturnStmt $ Just expr'
irgenStmt P.ReturnVoidStatement = return $ ReturnStmt Nothing
irgenStmt P.BreakStatement = return $ BreakStmt
irgenStmt P.ContinueStatement = return $ ContinueStmt

irgenExpr :: P.Expr -> Semantic Expr
irgenExpr (P.LocationExpr loc) = do
  loc' <- irgenLocation loc
  return $ LocationExpr loc'
irgenExpr (P.MethodCallExpr method) = do
  method' <- irgenMethod method
  return $ MethodCallExpr method'
irgenExpr (P.IntLiteralExpr i) = return $ IntLiteralExpr $ read $ B.toString i
irgenExpr (P.BoolLiteralExpr b) = return $ BoolLiteralExpr $ read $ B.toString b
irgenExpr (P.CharLiteralExpr c) = return $ CharLiteralExpr $ read $ B.toString c
irgenExpr (P.LenExpr id) = return $ LengthExpr id
irgenExpr (P.ArithOpExpr op l r) = do
  l' <- irgenExpr l
  r' <- irgenExpr r
  return $ BinaryOpExpr (parseBinaryOp op) l' r'
irgenExpr (P.RelOpExpr op l r) = do
  l' <- irgenExpr l
  r' <- irgenExpr r
  return $ BinaryOpExpr (parseBinaryOp op) l' r'
irgenExpr (P.EqOpExpr op l r) = do
  l' <- irgenExpr l
  r' <- irgenExpr r
  return $ BinaryOpExpr (parseBinaryOp op) l' r'
irgenExpr (P.CondOpExpr op l r) = do
  l' <- irgenExpr l
  r' <- irgenExpr r
  return $ BinaryOpExpr (parseBinaryOp op) l' r'
irgenExpr (P.NegativeExpr expr) = do
  expr' <- irgenExpr expr
  return $ UnaryOpExpr Negative expr'
irgenExpr (P.NegateExpr expr) = do
  expr' <- irgenExpr expr
  return $ UnaryOpExpr Negate expr'
irgenExpr (P.ParenExpr expr) = irgenExpr expr
irgenExpr (P.ChoiceExpr pred l r) = do
  pred' <- irgenExpr pred
  l' <- irgenExpr l
  r' <- irgenExpr r
  return $ TernaryOpExpr Choice pred' l' r'

irgenImportArg :: P.ImportArg -> Semantic Expr
irgenImportArg (P.ExprImportArg expr)  = irgenExpr expr
irgenImportArg (P.StringImportArg arg) = return $ StringLiteralExpr arg
