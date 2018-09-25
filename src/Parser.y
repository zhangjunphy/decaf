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
module Parser ( parse
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
  '{'               { ScannedToken _ _ LCurly }
  '}'               { ScannedToken _ _ RCurly }
  '['               { ScannedToken _ _ LBrack }
  ']'               { ScannedToken _ _ RBrack }
  '('               { ScannedToken _ _ LParen }
  ')'               { ScannedToken _ _ RParen }
  ';'               { ScannedToken _ _ Semicolon }
  ','               { ScannedToken _ _ Comma }
  import            { ScannedToken _ _ (Keyword "import") }
  int               { ScannedToken _ _ (Keyword "int") }
  bool              { ScannedToken _ _ (Keyword "bool") }
  void              { ScannedToken _ _ (Keyword "void") }

%% -------------------------------- Grammar -----------------------------------

Program : ImportDecls FieldDecls MethodDecls                { Program $1 $2 $3 }

ImportDecls : {- empty -}                                   { [] }
            | ImportDecls ImportDecl                        { $2 : $1 }
ImportDecl : import id ';'                                  { ImportDecl $2 }

FieldDecls : {- empty -}                                    { [] }
           | FieldDecls FieldDecl                           { $2 : $1 }
FieldDecl : Type FieldItemList ';'                          { FieldDecl $1 $2 }
Type : int                                                  { IntType }
     | bool                                                 { BoolType }
FieldItemList : FieldItem                                   { [$1] }
              | FieldItemList ',' FieldItem                 { $3 : $1 }
FieldItem : id                                              { FieldElem $1 }
          | id '[' intLiteral ']'                           { FieldArray $1 $3 }

MethodDecls : {- empty -}                                   { [] }
            | MethodDecls MethodDecl                        { $2 : $1 }
MethodDecl : MethodType id '(' ArgumentList ')' Block       { MethodDecl $2 $1 $4 $6 }
MethodType : Type                                           { $1 }
           | void                                           { VoidType }
ArgumentList : Argument                                     { [$1] }
             | ArgumentList ',' Argument                    { $3 : $1 }
Argument : Type id                                          { Argument $2 $1 }
Block : '{' FieldDecls '}'                                  { Block $2 }

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

data FieldItem = FieldElem { fieldId :: String }
               | FieldArray { fieldId :: String, size :: String }
                 deriving (Show)

data Type = IntType | BoolType | VoidType
            deriving (Show)

data MethodDecl = MethodDecl { methodId :: String
                             , returnType :: Type
                             , arguments :: [Argument]
                             , block :: Block
                             } deriving (Show)

data Argument = Argument { argumentId :: String
                         , argumentType :: Type
                         } deriving (Show)

data Block = Block { blockFieldDecls :: [FieldDecl]
                   } deriving (Show)

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
