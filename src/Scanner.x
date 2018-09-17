-- Scanner -- Decaf scanner                                     -*- haskell -*-
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
{-# OPTIONS_GHC -w #-}
module Scanner ( ScannedToken(..)
               , Token(..)
               , scan
               , formatTokenOrError
               ) where

import Control.Monad.State
}

%wrapper "6.035"


----------------------------------- Tokens ------------------------------------

-- binary operators
$arithOp = [\+ \- \* \/ \%]
@relOp = \<\= | \>\= | [\< \>]
@eqOp = \=\= | \!\=
@condOp = \&\& | \|\|
@binOp = $arithOp | @relOp | @eqOp | @condOp

-- alphabet and digits
$alpha = [a-zA-Z]
$digit = [0-9]
$alphaNum = [$alpha $digit]
$hexDigit = [$digit a-fA-F]
@specialChar = \\\" | \\\' | \\\\ | \\t | \\n
@char = $printable # [\" \' \\] | @specialChar

-- literals
@decimalLiteral = $digit+
@hexLiteral = 0x $hexDigit+
@boolLiteral = true | false
@charLiteral = \'@char\'
@stringLiteral = \"@char*\"
@intLiteral = @decimalLiteral | @hexLiteral
@literal = @intLiteral | @charLiteral | @boolLiteral

-- assign and increment
$assignOp = \=
@compoundAssignOp = \+\= | \-\=
@incrementOp = \+\+ | \-\-

-- identifiers and keywords
@id = [$alpha _] [$alphaNum _]*
@keyword = bool | break | import | continue | else | for | while | if
           | int | return | len | void

-- whitespaces
$white2 = $white # \f -- we want the scanner to error on '\f' (form feed) characters

-- keyword and identifier separators
$syntaxChars = [\; \, \: \? \! \{\} \[\] \(\) \= \+ \- \* \/ \$ \| \% $white2]


-- rules
tokens :-
  <0>             $white2+                   ;
  <0>             "//".*                     ;
  <0>             "/*"[.\n]*"*/"             ;
  <0>             $syntaxChars ^ @keyword    { \posn s -> scannedToken posn $ Keyword s }
  <0>             @charLiteral               { \posn s -> scannedToken posn $ CharLiteral s }
  <0>             @intLiteral                { \posn s -> scannedToken posn $ IntLiteral s }
  <0>             @boolLiteral               { \posn s -> scannedToken posn $ BooleanLiteral s }
  <0>             @stringLiteral             { \posn s -> scannedToken posn $ StringLiteral s }
  <0>             $syntaxChars ^ @id         { \posn s -> scannedToken posn $ Identifier s }
  <0>             $assignOp                  { \posn s -> scannedToken posn AssignOp }
  <0>             @compoundAssignOp          { \posn s -> scannedToken posn $ CompoundAssignOp s }
  <0>             $arithOp                   { \posn s -> scannedToken posn $ ArithmeticOp s }
  <0>             @relOp                     { \posn s -> scannedToken posn $ RelationOp s }
  <0>             @eqOp                      { \posn s -> scannedToken posn $ EquationOp s }
  <0>             @condOp                    { \posn s -> scannedToken posn $ ConditionOp s }
  <0>             \{                         { \posn _ -> scannedToken posn LCurly }
  <0>             \}                         { \posn _ -> scannedToken posn RCurly }
  <0>             \(                         { \posn _ -> scannedToken posn LParen }
  <0>             \)                         { \posn _ -> scannedToken posn RParen }
  <0>             \[                         { \posn _ -> scannedToken posn LBrack }
  <0>             \]                         { \posn _ -> scannedToken posn RBrack }
  <0>             \?                         { \posn _ -> scannedToken posn Choice }
  <0>             \:                         { \posn _ -> scannedToken posn Colon }
  <0>             \;                         { \posn _ -> scannedToken posn Semicolon }
  <0>             \,                         { \posn _ -> scannedToken posn Comma }
  <0>             \!                         { \posn _ -> scannedToken posn Negate }


----------------------------- Representing tokens -----------------------------

{
-- | A token with position information.
data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , extractRawToken :: Token
                                 } deriving (Eq)

-- | A token.
data Token = Keyword String
           | Identifier String
           | CharLiteral String
           | IntLiteral String
           | BooleanLiteral String
           | StringLiteral String
           | AssignOp
           | CompoundAssignOp String
           | ArithmeticOp String
           | RelationOp String
           | EquationOp String
           | ConditionOp String
           | LCurly
           | RCurly
           | LParen
           | RParen
           | LBrack
           | RBrack
           | Choice
           | Colon
           | Semicolon
           | Comma
           | Negate
           deriving (Eq)

instance Show Token where
  show (Keyword k) = k
  show (Identifier s) = "IDENTIFIER " ++ s
  show (CharLiteral s) = "CHARLITERAL " ++ s
  show (IntLiteral s) = "INTLITERAL " ++ s
  show (BooleanLiteral s) = "BOOLEANLITERAL " ++ s
  show (StringLiteral s) = "STRINGLITERAL " ++ s
  show AssignOp = "="
  show (CompoundAssignOp s) = s
  show (ArithmeticOp s) = s
  show (RelationOp s) = s
  show (EquationOp s) = s
  show (ConditionOp s) = s
  show LCurly = "{"
  show RCurly = "}"
  show LParen = "("
  show RParen = ")"
  show LBrack = "["
  show RBrack = "]"
  show Choice = "?"
  show Colon = ":"
  show Semicolon = ";"
  show Comma = ","
  show Negate = "!"

{-| Smart constructor to create a 'ScannedToken' by extracting the line and
column numbers from an 'AlexPosn'. -}
scannedToken :: AlexPosn -> Token -> ScannedToken
scannedToken (AlexPn _ lineNo columnNo) tok = ScannedToken lineNo columnNo tok

---------------------------- Scanning entry point -----------------------------

-- fill out this function with extra cases if you use error tokens
-- and want them to be treated as errors instead of valid tokens
catchErrors :: Either String ScannedToken -> Either String ScannedToken
catchErrors e = e -- default case

scan :: String -> [Either String ScannedToken]
scan = map catchErrors . alexScanTokens

formatTokenOrError :: Either String ScannedToken -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right tok) = Right $ unwords [ show $ line tok
                                                 , show $ extractRawToken tok
                                                 ]
}
