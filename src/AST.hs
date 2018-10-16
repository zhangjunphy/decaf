-- AST -- Decaf AST builder
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

module AST ( buildAst
           ) where

import Data.Either()
import Text.Read
import qualified Parser


-- AST tree nodes. --
data Type = Int | Bool
data AssignOp = EqualOp | IncreseEqualOp | DecreseEqualOp
data IncrementOp = IncrementOp | DecrementOp
data ArithOp = Plus | Minus | Multiply | Division
data RelationOp = LessThan | LessEqual | GreaterThan | GreaterEqual | Equals | NotEqual
data ConditionOp = And | Or

data AbstractSyntaxTree = AbstractSyntaxTree { importList :: [Import], fieldList :: [Field], methodList :: [Method] }
data Import = Import { id :: String }
data Field = ItemField { id :: String, tpe :: Type }
           | ArrayField { id :: String, tpe :: Type, size :: Int }
data Method = Method { id :: String
                     , returnType :: Maybe Type
                     , arguments :: [(String, Type)]
                     , fieldList :: [Field]
                     , body :: [Statement]
                     }
data Statement = AssignStatement { location :: Location, assignOp :: AssignOp, expr :: Expr }
               | IncrementAssignStatement { location :: Location, incrementOp :: IncrementOp}
               | MethodCall { name :: String, arguments :: [ImportArgument] }
data Expr = LocationExpr { location :: Location }
          | ArithExpr { arithOp :: ArithOp, lExpr :: Expr, rExpr :: Expr }
          | IntLiteralExpr { intLiteral :: Int }
          | BoolLiteralExpr { boolLiteral :: Bool }
          | LenExpr { id :: String}
          | RelationExpr { relationOp :: RelationOp, lExpr :: Expr, rExpr :: Expr }
          | ConditionExpr { conditionOp :: ConditionOp, lExpr :: Expr, rExpr :: Expr }
data Location = ScalarLocation { id :: String }
          | ArrayLocation { id :: String, index :: Expr }
data ImportArgument = ExprArgument { exprArgument :: Expr }
                     | StringArgument { stringargument :: String }

data Position = Position { line :: Int,
                           column :: Int
                         } deriving (Show)


-- Utilities --
extractErrorMessage :: Either [String] a -> [String]
extractErrorMessage (Left m) = m
extractErrorMessage (Right _) = []

flatten :: [Either b a] -> [a]
flatten [] = []
flatten (x:xs) = case x of
                   Left _ -> flatten xs
                   Right s -> [s] ++ flatten xs


-- convert parser types
fromParserType :: Parser.Type -> Type
fromParserType Parser.IntType = Int
fromParserType Parser.BoolType = Bool

fromParserArgument :: Parser.Argument -> (String, Type)
fromParserArgument a = (Parser.argumentId a, fromParserType $ Parser.argumentType a)

fromParserExpr :: Parser.Expr -> Expr
fromParserExpr (Parser.LocationExpr (Parser.ElemLocation loc)) =
    LocationExpr $ ScalarLocation loc
fromParserExpr (Parser.LocationExpr (Parser.ArrayLocation id' expr')) =
    LocationExpr $ ArrayLocation id' $ fromParserExpr expr'

fromParserAssignOp :: String -> Either String AssignOp
fromParserAssignOp "=" = Right EqualOp
fromParserAssignOp "+=" = Right IncreseEqualOp
fromParserAssignOp "-=" = Right DecreseEqualOp
fromParserAssignOp o = Left $ "Unrecognized operator: " ++ o

fromParserIncOp :: String -> Either String IncrementOp
fromParserIncOp "++" = Right IncrementOp
fromParserIncOp "--" = Right DecrementOp
fromParserIncOp o = Left $ "Unrecognized operator: " ++ o


-- Actually build the AST --
buildAst :: Parser.Program -> Either [String] AbstractSyntaxTree
buildAst program =
    if null errors
    then Right $ AbstractSyntaxTree $ Root importAst fieldAst methodAst
    else Left errors
  where imports = fmap buildImport $ Parser.importDecls program
        fields  = concat $ fmap buildField $ Parser.fieldDecls program
        methods = fmap buildMethod $ Parser.methodDecls program
        importAst = flatten imports
        fieldAst = flatten fields
        methodAst = flatten methods
        importErrors = map extractErrorMessage imports
        fieldErrors = map extractErrorMessage fields
        methodErrors = map extractErrorMessage methods
        errors = concat $ importErrors ++ fieldErrors ++ methodErrors


buildImport :: Parser.ImportDecl -> Either [String] Import
buildImport x =  Right $ Import $ Parser.importId x

buildField :: Parser.FieldDecl -> [Either [String] Field]
buildField f = fmap (getItem $ fromParserType $ Parser.fieldType f) $ Parser.items f
    where getItem tpe (Parser.ElemField fieldId) = Right $ ItemField fieldId tpe
          getItem tpe (Parser.ArrayField fieldId sz) =
              case fmap (ArrayField fieldId tpe) (readMaybe sz) of
                Just field -> Right field
                Nothing -> Left [("Array size should be an integer: " ++ sz)]

buildStatement :: Parser.Statement -> Either [String] Statement
buildStatement (Parser.AssignStatement (Parser.ElemLocation loc) (Parser.AssignExpr op expr)) =
    case fromParserAssignOp op of
      Right o -> Right $ AssignStatement (ScalarLocation loc) o (fromParserExpr expr)
      Left m -> Left [m]
buildStatement (Parser.AssignStatement (Parser.ElemLocation loc) (Parser.IncrementExpr op)) =
    case fromParserIncOp op of
      Right o -> Right $ IncrementAssignStatement (ScalarLocation loc) o
      Left m -> Left [m]

buildMethod :: Parser.MethodDecl -> Either [String] Method
buildMethod m =
    case errors of
      [] -> Right $ Method (Parser.methodId m) (fmap fromParserType $ Parser.returnType m)
            args (flatten fields) (flatten statements)
      _  -> Left errors
    where args = fmap fromParserArgument $ Parser.arguments m
          fields = concat $ fmap buildField $ Parser.blockFieldDecls $ Parser.block m
          statements = fmap buildStatement $ Parser.blockStatements $ Parser.block m
          errors = concat $ (fmap extractErrorMessage fields) ++ (fmap extractErrorMessage statements)
