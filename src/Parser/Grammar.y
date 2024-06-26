-- -*- haskell -*-
-- Copyright (C) 2018-2024 Jun Zhang <zhangjunphy[at]gmail[dot]com>
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
module Parser.Grammar where

import Util.SourceLoc as SL
import Parser.Tree
import Parser.Helper
import Lexer (Alex (..), Token (..))
}

--------------------------------- Directives ----------------------------------

%name parseInternal
%error { parseError }
%monad { Alex }
%lexer { lexerwrap } { SL.LocatedAt _ EOF }
%tokentype { SL.Located Token }

{-
Methods generated by Happy

parseInternal :: Alex Program
parseError :: Token -> Alex a
-}

%token
  id                { SL.LocatedAt _ (Identifier _) }

  intLiteral        { SL.LocatedAt _ (IntLiteral _) }
  stringLiteral     { SL.LocatedAt _ (StringLiteral _) }
  boolLiteral       { SL.LocatedAt _ (BooleanLiteral _) }
  charLiteral       { SL.LocatedAt _ (CharLiteral _) }

  '{'               { SL.LocatedAt _ (LCurly) }
  '}'               { SL.LocatedAt _ (RCurly) }
  '['               { SL.LocatedAt _ (LBrack) }
  ']'               { SL.LocatedAt _ (RBrack) }
  '('               { SL.LocatedAt _ (LParen) }
  ')'               { SL.LocatedAt _ (RParen) }
  ';'               { SL.LocatedAt _ (Semicolon) }
  '\:'              { SL.LocatedAt _ (Colon) }
  ','               { SL.LocatedAt _ (Comma) }
  '!'               { SL.LocatedAt _ (Negate) }
  '?'               { SL.LocatedAt _ (Choice) }

  import            { SL.LocatedAt _ (Keyword "import") }
  int               { SL.LocatedAt _ (Keyword "int") }
  bool              { SL.LocatedAt _ (Keyword "bool") }
  void              { SL.LocatedAt _ (Keyword "void") }
  if                { SL.LocatedAt _ (Keyword "if") }
  else              { SL.LocatedAt _ (Keyword "else") }
  for               { SL.LocatedAt _ (Keyword "for") }
  while             { SL.LocatedAt _ (Keyword "while") }
  return            { SL.LocatedAt _ (Keyword "return") }
  break             { SL.LocatedAt _ (Keyword "break") }
  continue          { SL.LocatedAt _ (Keyword "continue") }
  len               { SL.LocatedAt _ (Keyword "len") }

  '='               { SL.LocatedAt _ (AssignOp) }
  '+'               { SL.LocatedAt _ (ArithmeticOp "+") }
  '-'               { SL.LocatedAt _ (ArithmeticOp "-") }
  '*'               { SL.LocatedAt _ (ArithmeticOp "*") }
  '/'               { SL.LocatedAt _ (ArithmeticOp "/") }
  '%'               { SL.LocatedAt _ (ArithmeticOp "%") }
  '<'               { SL.LocatedAt _ (RelationOp "<") }
  '<='              { SL.LocatedAt _ (RelationOp "<=") }
  '>'               { SL.LocatedAt _ (RelationOp ">") }
  '>='              { SL.LocatedAt _ (RelationOp ">=") }
  '=='              { SL.LocatedAt _ (EquationOp "==") }
  '!='              { SL.LocatedAt _ (EquationOp "!=") }
  '&&'              { SL.LocatedAt _ (ConditionOp "&&") }
  '||'              { SL.LocatedAt _ (ConditionOp "||") }
  incrementOp       { SL.LocatedAt _ (IncrementOp _) }
  compoundAssignOp  { SL.LocatedAt _ (CompoundAssignOp _) }


-- precedence --
%right '+=' '-=' TERNARY_COND
%left '||'
%left '&&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left '!'
%left NEG
%left '++'

%% -------------------------------- Grammar -----------------------------------

Program : ImportDecls FieldDecls MethodDecls                { Program (reverse $1) (reverse $2) $3 }

ImportDecls : {- empty -}                                   { [] }

            | ImportDecls ImportDecl                        { $2 : $1 }
ImportDecl : import id ';'                                  { SL.LocatedAt (unionOf $1 $3) $ ImportDecl $ getID (unLoc $2) }

FieldDecls : {- empty -}                                    { [] }
           | FieldDecls FieldDecl                           { $2 : $1 }
FieldDecl : int FieldList ';'                               { SL.LocatedAt (unionOf $1 $3) $ FieldDecl IntType (reverse $2) }
          | bool FieldList ';'                              { SL.LocatedAt (unionOf $1 $3) $ FieldDecl BoolType (reverse $2) }

FieldList : FieldElem                                       { [$1] }
          | FieldList ',' FieldElem                         { $3 : $1 }
FieldElem : id                                              { SL.LocatedAt (getLoc $1) $ ScalarField $ getID (unLoc $1) }
          | id '[' intLiteral ']'                           { SL.LocatedAt (unionOf $1 $4) $ VectorField (getID $ unLoc $1) (getLiteral $ unLoc $3) }

MethodDecls : {- empty -}                                   { [] }
            | MethodDecl MethodDecls                        { $1 : $2 }
MethodDecl : int id '(' ArgumentList ')' Block              { SL.LocatedAt (unionOf $1 $6) $ MethodDecl (getID $ unLoc $2) (Just IntType) (reverse $4) (unLoc $6) }
           | bool id '(' ArgumentList ')' Block             { SL.LocatedAt (unionOf $1 $6) $ MethodDecl (getID $ unLoc $2) (Just BoolType) (reverse $4) (unLoc $6) }
           | void id '(' ArgumentList ')' Block             { SL.LocatedAt (unionOf $1 $6) $ MethodDecl (getID $ unLoc $2) Nothing (reverse $4) (unLoc $6) }
           | int id '(' ')' Block                           { SL.LocatedAt (unionOf $1 $5) $ MethodDecl (getID $ unLoc $2) (Just IntType) [] (unLoc $5) }
           | bool id '(' ')' Block                          { SL.LocatedAt (unionOf $1 $5) $ MethodDecl (getID $ unLoc $2) (Just BoolType) [] (unLoc $5) }
           | void id '(' ')' Block                          { SL.LocatedAt (unionOf $1 $5) $ MethodDecl (getID $ unLoc $2) Nothing [] (unLoc $5) }

ArgumentList : Argument                                     { [$1] }
             | ArgumentList ',' Argument                    { $3 : $1 }
Argument : int id                                           { SL.LocatedAt (unionOf $1 $2) $ Argument (getID $ unLoc $2) IntType }
         | bool id                                          { SL.LocatedAt (unionOf $1 $2) $ Argument (getID $ unLoc $2) BoolType }

Block : '{' FieldDecls Statements '}'                       { SL.LocatedAt (unionOf $1 $4) $ Block $2 (reverse $3) }

Statements : {- empty -}                                    { [] }
           | Statements Statement                           { $2 : $1 }

Statement : Location AssignExpr ';'                         { SL.LocatedAt (unionOf $1 $3) $ AssignStatement (unLoc $1) (unLoc $2) }
          | MethodCall ';'                                  { SL.LocatedAt (unionOf $1 $2) $ MethodCallStatement (unLoc $1) }
          | if '(' Expr ')' Block                           { SL.LocatedAt (unionOf $1 $5) $ IfStatement $3 (unLoc $5) }
          | if '(' Expr ')' Block else Block                { SL.LocatedAt (unionOf $1 $7) $ IfElseStatement $3 (unLoc $5) (unLoc $7) }
          | for '(' id '=' Expr ';' Expr ';' CounterUpdate ')' Block    { SL.LocatedAt (unionOf $1 $11) $ ForStatement (getID $ unLoc $3) $5 $7 (unLoc $9) (unLoc $11) }
          | while '(' Expr ')' Block                        { SL.LocatedAt (unionOf $1 $5) $ WhileStatement $3 (unLoc $5) }
          | return ';'                                      { SL.LocatedAt (unionOf $1 $2) $ ReturnVoidStatement }
          | return Expr ';'                                 { SL.LocatedAt (unionOf $1 $3) $ ReturnExprStatement $2 }
          | break ';'                                       { SL.LocatedAt (unionOf $1 $2) $ BreakStatement }
          | continue ';'                                    { SL.LocatedAt (unionOf $1 $2) $ ContinueStatement }

CounterUpdate : Location AssignExpr                         { SL.LocatedAt (unionOf $1 $2) $ CounterUpdate (unLoc $1) (unLoc $2) }

Location : id                                               { SL.LocatedAt (getLoc $1) $ ScalarLocation (getID $ unLoc $1) }
         | id '[' Expr ']'                                  { SL.LocatedAt (unionOf $1 $4) $ VectorLocation (getID $ unLoc $1) $3 }

AssignExpr : AssignOp Expr                                  { SL.LocatedAt (unionOf $1 $2) $ AssignExpr (unLoc $1) $2 }
           | incrementOp                                    { SL.LocatedAt (getLoc $1) $ IncrementExpr (getOp $ unLoc $1) }

AssignOp : '='                                              { SL.LocatedAt (getLoc $1) "=" }
         | compoundAssignOp                                 { SL.LocatedAt (getLoc $1) (getOp $ unLoc $1) }

MethodCall : id '(' ImportArgs ')'                          { SL.LocatedAt (unionOf $1 $4) $ MethodCall (getID $ unLoc $1) (reverse $3) }
           | id '(' ')'                                     { SL.LocatedAt (unionOf $1 $3) $ MethodCall (getID $ unLoc $1) [] }

ImportArgs : ImportArg                                      { [$1] }
           | ImportArgs ',' ImportArg                       { $3 : $1 }

ImportArg : Expr                                            { SL.LocatedAt (getLoc $1) $ ExprImportArg $1 }
          | stringLiteral                                   { SL.LocatedAt (getLoc $1) $ StringImportArg (getLiteral $ unLoc $1) }

Expr : Expr1                                                { SL.LocatedAt (getLoc $1) $ unLoc $1 }
     | Expr1 '?' Expr1 '\:' Expr %prec TERNARY_COND         { SL.LocatedAt (unionOf $1 $5) $ ChoiceExpr $1 $3 $5 }

Expr1 : Location                                            { SL.LocatedAt (getLoc $1) $ LocationExpr (unLoc $1) }
      | MethodCall                                          { SL.LocatedAt (getLoc $1) $ MethodCallExpr (unLoc $1) }
      | intLiteral                                          { SL.LocatedAt (getLoc $1) $ IntLiteralExpr (getLiteral $ unLoc $1) }
      | charLiteral                                         { SL.LocatedAt (getLoc $1) $ CharLiteralExpr (getLiteral $ unLoc $1) }
      | boolLiteral                                         { SL.LocatedAt (getLoc $1) $ BoolLiteralExpr (getLiteral $ unLoc $1) }
      | len '(' id ')'                                      { SL.LocatedAt (unionOf $1 $4) $ LenExpr (getID $ unLoc $3) }
      | Expr1 '+' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ ArithOpExpr "+" $1 $3 }
      | Expr1 '-' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ ArithOpExpr "-" $1 $3 }
      | Expr1 '*' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ ArithOpExpr "*" $1 $3 }
      | Expr1 '/' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ ArithOpExpr "/" $1 $3 }
      | Expr1 '%' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ ArithOpExpr "%" $1 $3 }
      | Expr1 '<' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ RelOpExpr "<" $1 $3 }
      | Expr1 '<=' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ RelOpExpr "<=" $1 $3 }
      | Expr1 '>' Expr1                                     { SL.LocatedAt (unionOf $1 $3) $ RelOpExpr ">" $1 $3 }
      | Expr1 '>=' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ RelOpExpr ">=" $1 $3 }
      | Expr1 '==' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ EqOpExpr "==" $1 $3 }
      | Expr1 '!=' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ EqOpExpr "!=" $1 $3 }
      | Expr1 '&&' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ CondOpExpr "&&" $1 $3 }
      | Expr1 '||' Expr1                                    { SL.LocatedAt (unionOf $1 $3) $ CondOpExpr "||" $1 $3 }
      | '-' Expr1 %prec NEG                                 { SL.LocatedAt (unionOf $1 $2) $ NegativeExpr $2 }
      | '!' Expr1                                           { SL.LocatedAt (unionOf $1 $2) $ NegateExpr $2 }
      | '(' Expr ')'                                        { SL.LocatedAt (unionOf $1 $3) $ ParenExpr $2 }
