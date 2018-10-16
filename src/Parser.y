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

module Parser ( parse
              , Program(..)
              , ImportDecl(..)
              , FieldDecl(..)
              , MethodDecl(..)
              , FieldItem(..)
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

import Scanner (ScannedToken(..), Token(..))

}


--------------------------------- Directives ----------------------------------

%name parse
%error { parseError }
%monad { Either String }

%tokentype { ScannedToken }

%token
  id                { ScannedToken _ _ (Identifier $$) }

  intLiteral        { ScannedToken _ _ (IntLiteral $$) }
  stringLiteral     { ScannedToken _ _ (StringLiteral $$) }
  boolLiteral       { ScannedToken _ _ (BooleanLiteral $$) }
  charLiteral       { ScannedToken _ _ (CharLiteral $$) }

  '{'               { ScannedToken _ _ LCurly }
  '}'               { ScannedToken _ _ RCurly }
  '['               { ScannedToken _ _ LBrack }
  ']'               { ScannedToken _ _ RBrack }
  '('               { ScannedToken _ _ LParen }
  ')'               { ScannedToken _ _ RParen }
  ';'               { ScannedToken _ _ Semicolon }
  '\:'              { ScannedToken _ _ Colon }
  ','               { ScannedToken _ _ Comma }
  '!'               { ScannedToken _ _ Negate }
  '?'               { ScannedToken _ _ Choice }

  import            { ScannedToken _ _ (Keyword "import") }
  int               { ScannedToken _ _ (Keyword "int") }
  bool              { ScannedToken _ _ (Keyword "bool") }
  void              { ScannedToken _ _ (Keyword "void") }
  if                { ScannedToken _ _ (Keyword "if") }
  else              { ScannedToken _ _ (Keyword "else") }
  for               { ScannedToken _ _ (Keyword "for") }
  while             { ScannedToken _ _ (Keyword "while") }
  return            { ScannedToken _ _ (Keyword "return") }
  break             { ScannedToken _ _ (Keyword "break") }
  continue          { ScannedToken _ _ (Keyword "continue") }
  len               { ScannedToken _ _ (Keyword "len") }

  '='               { ScannedToken _ _ AssignOp }
  '+'               { ScannedToken _ _ (ArithmeticOp "+") }
  '-'               { ScannedToken _ _ (ArithmeticOp "-") }
  '*'               { ScannedToken _ _ (ArithmeticOp "*") }
  '/'               { ScannedToken _ _ (ArithmeticOp "/") }
  '%'               { ScannedToken _ _ (ArithmeticOp "%") }
  '<'               { ScannedToken _ _ (RelationOp "<") }
  '<='              { ScannedToken _ _ (RelationOp "<=") }
  '>'               { ScannedToken _ _ (RelationOp ">") }
  '>='              { ScannedToken _ _ (RelationOp ">=") }
  '=='              { ScannedToken _ _ (EquationOp "==") }
  '!='              { ScannedToken _ _ (EquationOp "!=") }
  '&&'              { ScannedToken _ _ (ConditionOp "&&") }
  '||'              { ScannedToken _ _ (ConditionOp "||") }
  incrementOp       { ScannedToken _ _ (IncrementOp $$) }
  compoundAssignOp  { ScannedToken _ _ (CompoundAssignOp $$) }


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
FieldDecl : int FieldItemList ';'                           { FieldDecl IntType (reverse $2) }
          | bool FieldItemList ';'                          { FieldDecl BoolType (reverse $2) }

FieldItemList : FieldItem                                   { [$1] }
              | FieldItemList ',' FieldItem                 { $3 : $1 }
FieldItem : id                                              { ElemField $1 }
          | id '[' intLiteral ']'                           { ArrayField $1 $3 }

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

Location : id                                               { ElemLocation $1 }
         | id '[' Expr ']'                                  { ArrayLocation $1 $3 }

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

data Program = Program { importDecls :: [ImportDecl]
                       , fieldDecls :: [FieldDecl]
                       , methodDecls :: [MethodDecl]
                       } deriving (Show)

data ImportDecl = ImportDecl { importId :: String }
                  deriving (Show)

data FieldDecl = FieldDecl { fieldType :: Type
                           , items :: [FieldItem]
                           } deriving (Show)

data FieldItem = ElemField { fieldId :: String }
               | ArrayField { fieldId :: String, size :: String }
                 deriving (Show)

data Type = IntType | BoolType
            deriving (Show)

data MethodDecl = MethodDecl { methodId :: String
                             , returnType :: Maybe Type
                             , arguments :: [Argument]
                             , block :: Block
                             } deriving (Show)

data Argument = Argument { argumentId :: String
                         , argumentType :: Type
                         } deriving (Show)

data Block = Block { blockFieldDecls :: [FieldDecl]
                   , blockStatements :: [Statement]
                   } deriving (Show)

data Statement = AssignStatement { assignLocation :: Location, assignExpr :: AssignExpr }
               | MethodCallStatement { methodCallStatement :: MethodCall }
               | IfStatement { ifExpr :: Expr, ifBlock :: Block }
               | IfElseStatement { ifExpr :: Expr, ifBlock :: Block, elseBlock :: Block}
               | ForStatement { counterId :: String, counterExpr :: Expr, forPredExpr :: Expr, counterUpdate :: CounterUpdate, forBlock :: Block }
               | WhileStatement { whileExpr :: Expr, whileBlock :: Block }
               | ReturnVoidStatement
               | ReturnExprStatement { returnExpr :: Expr }
               | BreakStatement
               | ContinueStatement
                 deriving (Show)

data Location = ElemLocation { locationId :: String }
              | ArrayLocation { locationId :: String, arrayIndexExpr :: Expr }
                deriving (Show)

data AssignExpr = AssignExpr { assignOp :: String, assignSourceExpr:: Expr }
                | IncrementExpr { incrementOp :: String }
                  deriving (Show)

data MethodCall = MethodCall { methodName :: String, importArguments :: [ImportArg] }
                  deriving (Show)

data ImportArg = ExprImportArg { argumentExpr :: Expr }
               | StringImportArg { argumentString :: String }
                 deriving (Show)

data CounterUpdate = CounterUpdate { counterLocation :: Location, updateExpr :: AssignExpr }
                     deriving (Show)

data Expr = LocationExpr { location :: Location }
          | MethodCallExpr { methodCallExpr :: MethodCall }
          | IntLiteralExpr { intLiteral :: String }
          | CharLiteralExpr { charLiteral :: String }
          | BoolLiteralExpr { boolLiteral :: String }
          | LenExpr { lenId :: String }
          | ArithOpExpr { arithOp :: String, lExpr :: Expr, rExpr :: Expr }
          | RelOpExpr { relOp :: String, lExpr :: Expr, rExpr :: Expr }
          | EqOpExpr { eqOp :: String, lExpr :: Expr, rExpr :: Expr }
          | CondOpExpr { condOp :: String, lExpr :: Expr, rExpr :: Expr }
          | NegativeExpr { negativeExpr :: Expr }
          | NegateExpr { negateExpr :: Expr }
          | ParenExpr { parenExpr :: Expr }
          | ChoiceExpr { choicePredExpr :: Expr, lExpr :: Expr, rExpr :: Expr }
            deriving (Show)

parseError :: [ScannedToken] -> Either String a
parseError [] = Left "unexpected EOF"
parseError toks =
  Left $ printf "line %d:%d: unexpected token%s '%s'"
                lineNo
                columnNo
                (if (not $ null $ tail toks) then "s" else "")
                badTokenText
  where firstBadToken = head toks
        lineNo = Scanner.line firstBadToken
        columnNo = Scanner.column firstBadToken
        badTokenText = concatMap (show . extractRawToken) toks
}
