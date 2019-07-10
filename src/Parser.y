-- Parser -- Decaf parser                                       -*- haskell -*-
-- Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
{
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser ( parse
              , Program(..)
              , ImportDecl(..)
              , FieldDecl(..)
              , MethodDecl(..)
              , FieldElem(..)
              , Type(..)
              , Argument(..)
              , Block(..)
              , Statement(..)
              , Location(..)
              , AssignExpr(..)
              , MethodCall(..)
              , ImportArg(..)
              , CounterUpdate(..)
              , Expr(..)
              ) where

import Text.Printf (printf)

import Scanner ( Token(..)
               , Alex(..)
               , AlexPosn(..)
               , runAlex
               , alexMonadScan
               , getLexerPosn
               )

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B (fromString, toString)
import qualified Data.ByteString.Lazy.Char8 as C8
}

--------------------------------- Directives ----------------------------------

%name parseInternal
%error { parseError }
%monad { Alex }
%lexer { lexerwrap } { EOF }
%tokentype { Token }

%token
  id                { (Identifier $$) }

  intLiteral        { (IntLiteral $$) }
  stringLiteral     { (StringLiteral $$) }
  boolLiteral       { (BooleanLiteral $$) }
  charLiteral       { (CharLiteral $$) }

  '{'               { LCurly }
  '}'               { RCurly }
  '['               { LBrack }
  ']'               { RBrack }
  '('               { LParen }
  ')'               { RParen }
  ';'               { Semicolon }
  '\:'              { Colon }
  ','               { Comma }
  '!'               { Negate }
  '?'               { Choice }

  import            { (Keyword "import") }
  int               { (Keyword "int") }
  bool              { (Keyword "bool") }
  void              { (Keyword "void") }
  if                { (Keyword "if") }
  else              { (Keyword "else") }
  for               { (Keyword "for") }
  while             { (Keyword "while") }
  return            { (Keyword "return") }
  break             { (Keyword "break") }
  continue          { (Keyword "continue") }
  len               { (Keyword "len") }

  '='               { AssignOp }
  '+'               { (ArithmeticOp "+") }
  '-'               { (ArithmeticOp "-") }
  '*'               { (ArithmeticOp "*") }
  '/'               { (ArithmeticOp "/") }
  '%'               { (ArithmeticOp "%") }
  '<'               { (RelationOp "<") }
  '<='              { (RelationOp "<=") }
  '>'               { (RelationOp ">") }
  '>='              { (RelationOp ">=") }
  '=='              { (EquationOp "==") }
  '!='              { (EquationOp "!=") }
  '&&'              { (ConditionOp "&&") }
  '||'              { (ConditionOp "||") }
  incrementOp       { (IncrementOp $$) }
  compoundAssignOp  { (CompoundAssignOp $$) }


-- precedence --
%left '||'
%left '&&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left '!'
%left NEG

%% -------------------------------- Grammar -----------------------------------

Program : ImportDecls FieldDecls MethodDecls                { Program (reverse $1) (reverse $2) $3 }

ImportDecls : {- empty -}                                   { [] }
            | ImportDecls ImportDecl                        { $2 : $1 }
ImportDecl : import id ';'                                  { ImportDecl $2 }

FieldDecls : {- empty -}                                    { [] }
           | FieldDecls FieldDecl                           { $2 : $1 }
FieldDecl : int FieldList ';'                               { FieldDecl IntType (reverse $2) }
          | bool FieldList ';'                              { FieldDecl BoolType (reverse $2) }

FieldList : FieldElem                                       { [$1] }
          | FieldList ',' FieldElem                         { $3 : $1 }
FieldElem : id                                              { ScalarField $1 }
          | id '[' intLiteral ']'                           { VectorField $1 $3 }

MethodDecls : {- empty -}                                   { [] }
            | MethodDecl MethodDecls                        { $1 : $2 }
MethodDecl : int id '(' ArgumentList ')' Block              { MethodDecl $2 (Just IntType) (reverse $4) $6 }
           | bool id '(' ArgumentList ')' Block             { MethodDecl $2 (Just BoolType) (reverse $4) $6 }
           | void id '(' ArgumentList ')' Block             { MethodDecl $2 Nothing (reverse $4) $6 }
           | int id '(' ')' Block                           { MethodDecl $2 (Just IntType) [] $5 }
           | bool id '(' ')' Block                          { MethodDecl $2 (Just BoolType) [] $5 }
           | void id '(' ')' Block                          { MethodDecl $2 Nothing [] $5 }

ArgumentList : Argument                                     { [$1] }
             | ArgumentList ',' Argument                    { $3 : $1 }
Argument : int id                                           { Argument $2 IntType }
         | bool id                                          { Argument $2 BoolType }

Block : '{' FieldDecls Statements '}'                       { Block $2 (reverse $3) }

Statements : {- empty -}                                    { [] }
           | Statements Statement                           { $2 : $1 }

Statement : Location AssignExpr ';'                         { AssignStatement $1 $2 }
          | MethodCall ';'                                  { MethodCallStatement $1 }
          | if '(' Expr ')' Block                           { IfStatement $3 $5 }
          | if '(' Expr ')' Block else Block                { IfElseStatement $3 $5 $7 }
          | for '(' id '=' Expr ';' Expr ';' CounterUpdate ')' Block    { ForStatement $3 $5 $7 $9 $11 }
          | while '(' Expr ')' Block                        { WhileStatement $3 $5 }
          | return ';'                                      { ReturnVoidStatement }
          | return Expr ';'                                 { ReturnExprStatement $2 }
          | break ';'                                       { BreakStatement }
          | continue ';'                                    { ContinueStatement }

CounterUpdate : Location AssignExpr                         { CounterUpdate $1 $2 }

Location : id                                               { ScalarLocation $1 }
         | id '[' Expr ']'                                  { VectorLocation $1 $3 }

AssignExpr : AssignOp Expr                                  { AssignExpr $1 $2 }
           | incrementOp                                    { IncrementExpr $1 }

AssignOp : '='                                              { "=" }
         | compoundAssignOp                                 { $1 }

MethodCall : id '(' ImportArgs ')'                          { MethodCall $1 (reverse $3) }
           | id '(' ')'                                     { MethodCall $1 [] }

ImportArgs : ImportArg                                      { [$1] }
           | ImportArgs ',' ImportArg                       { $3 : $1 }

ImportArg : Expr                                            { ExprImportArg $1 }
          | stringLiteral                                   { StringImportArg $1 }

Expr : Expr1                                                { $1 }
     | Expr1 '?' Expr1 '\:' Expr                            { ChoiceExpr $1 $3 $5 }

Expr1 : Location                                            { LocationExpr $1 }
      | MethodCall                                          { MethodCallExpr $1 }
      | intLiteral                                          { IntLiteralExpr $1 }
      | charLiteral                                         { CharLiteralExpr $1 }
      | boolLiteral                                         { BoolLiteralExpr $1 }
      | len '(' id ')'                                      { LenExpr $3 }
      | Expr1 '+' Expr1                                     { ArithOpExpr "+" $1 $3 }
      | Expr1 '-' Expr1                                     { ArithOpExpr "-" $1 $3 }
      | Expr1 '*' Expr1                                     { ArithOpExpr "*" $1 $3 }
      | Expr1 '/' Expr1                                     { ArithOpExpr "/" $1 $3 }
      | Expr1 '%' Expr1                                     { ArithOpExpr "%" $1 $3 }
      | Expr1 '<' Expr1                                     { RelOpExpr "<" $1 $3 }
      | Expr1 '<=' Expr1                                    { RelOpExpr "<=" $1 $3 }
      | Expr1 '>' Expr1                                     { RelOpExpr ">" $1 $3 }
      | Expr1 '>=' Expr1                                    { RelOpExpr ">=" $1 $3 }
      | Expr1 '==' Expr1                                    { EqOpExpr "==" $1 $3 }
      | Expr1 '!=' Expr1                                    { EqOpExpr "!=" $1 $3 }
      | Expr1 '&&' Expr1                                    { CondOpExpr "&&" $1 $3 }
      | Expr1 '||' Expr1                                    { CondOpExpr "||" $1 $3 }
      | '-' Expr1 %prec NEG                                 { NegativeExpr $2 }
      | '!' Expr1                                           { NegateExpr $2 }
      | '(' Expr1 ')'                                       { ParenExpr $2 }

----------------------------------- Haskell -----------------------------------
{

lexerwrap :: (Token -> Alex a) -> Alex a
lexerwrap s = do
  token <- alexMonadScan
  s token

data Program = Program { importDecls :: [ImportDecl]
                       , fieldDecls :: [FieldDecl]
                       , methodDecls :: [MethodDecl]
                       } deriving (Show)

data ImportDecl = ImportDecl { importId :: ByteString }
                  deriving (Show)

data FieldDecl = FieldDecl { fieldType :: Type
                           , elems:: [FieldElem]
                           } deriving (Show)

data FieldElem = ScalarField { fieldId :: ByteString }
               | VectorField { fieldId :: ByteString, size :: ByteString }
                 deriving (Show)

data Type = IntType | BoolType
            deriving (Show)

data MethodDecl = MethodDecl { methodId :: ByteString
                             , returnType :: Maybe Type
                             , arguments :: [Argument]
                             , block :: Block
                             } deriving (Show)

data Argument = Argument { argumentId :: ByteString
                         , argumentType :: Type
                         } deriving (Show)

data Block = Block { blockFieldDecls :: [FieldDecl]
                   , blockStatements :: [Statement]
                   } deriving (Show)

data Statement = AssignStatement { assignLocation :: Location, assignExpr :: AssignExpr }
               | MethodCallStatement { methodCallStatement :: MethodCall }
               | IfStatement { ifExpr :: Expr, ifBlock :: Block }
               | IfElseStatement { ifExpr :: Expr, ifBlock :: Block, elseBlock :: Block}
               | ForStatement { counterId :: ByteString, counterExpr :: Expr, forPredExpr :: Expr, counterUpdate :: CounterUpdate, forBlock :: Block }
               | WhileStatement { whileExpr :: Expr, whileBlock :: Block }
               | ReturnVoidStatement
               | ReturnExprStatement { returnExpr :: Expr }
               | BreakStatement
               | ContinueStatement
                 deriving (Show)

data Location = ScalarLocation { locationId :: ByteString }
              | VectorLocation { locationId :: ByteString, arrayIndexExpr :: Expr }
                deriving (Show)

data AssignExpr = AssignExpr { assignOp :: ByteString, assignSourceExpr:: Expr }
                | IncrementExpr { incrementOp :: ByteString }
                  deriving (Show)

data MethodCall = MethodCall { methodName :: ByteString, importArguments :: [ImportArg] }
                  deriving (Show)

data ImportArg = ExprImportArg { argumentExpr :: Expr }
               | StringImportArg { argumentString :: ByteString }
                 deriving (Show)

data CounterUpdate = CounterUpdate { counterLocation :: Location, updateExpr :: AssignExpr }
                     deriving (Show)

data Expr = LocationExpr { location :: Location }
          | MethodCallExpr { methodCallExpr :: MethodCall }
          | IntLiteralExpr { intLiteral :: ByteString }
          | CharLiteralExpr { charLiteral :: ByteString }
          | BoolLiteralExpr { boolLiteral :: ByteString }
          | LenExpr { lenId :: ByteString }
          | ArithOpExpr { arithOp :: ByteString, lExpr :: Expr, rExpr :: Expr }
          | RelOpExpr { relOp :: ByteString, lExpr :: Expr, rExpr :: Expr }
          | EqOpExpr { eqOp :: ByteString, lExpr :: Expr, rExpr :: Expr }
          | CondOpExpr { condOp :: ByteString, lExpr :: Expr, rExpr :: Expr }
          | NegativeExpr { negativeExpr :: Expr }
          | NegateExpr { negateExpr :: Expr }
          | ParenExpr { parenExpr :: Expr }
          | ChoiceExpr { choicePredExpr :: Expr, lExpr :: Expr, rExpr :: Expr }
          | ErrorExpr
            deriving (Show)

parse :: ByteString -> Either String Program
parse input = runAlex input parseInternal

parseError :: Token -> Alex a
parseError tok = do
  (AlexPn _ line col) <- getLexerPosn
  Alex $ \_ -> Left $ printf "%d:%d: Error handling token '%s'" line col (show tok)
}
