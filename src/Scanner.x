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

import Data.Maybe
import Control.Monad.State
}

%wrapper "monadUserState"


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

-- invalid characters for better error handling
$invalid = $printable # [a-z A-Z 0-9 _ $syntaxChars]

-- rules
tokens :-
  <0>             $white2+                   ;
  <0>             "//".*                     ;
  <0, inComment>  "/*"                       { enterComment `andBegin` inComment }
  <inComment>     "*/"                       { exitComment }
  <inComment>     [.\n]                      ;
  <0>             $syntaxChars ^ @keyword    { \inp len -> scannedToken inp $ Keyword (extractTokenString inp len) }
  <0>             @charLiteral               { \inp len -> scannedToken inp $ CharLiteral (extractTokenString inp len) }
  <0>             @intLiteral                { \inp len -> scannedToken inp $ IntLiteral (extractTokenString inp len) }
  <0>             @boolLiteral               { \inp len -> scannedToken inp $ BooleanLiteral (extractTokenString inp len) }
  <0>             @stringLiteral             { \inp len -> scannedToken inp $ StringLiteral (extractTokenString inp len) }
  <0>             $syntaxChars ^ @id         { \inp len -> scannedToken inp $ Identifier (extractTokenString inp len) }
  <0>             $assignOp                  { \inp len -> scannedToken inp AssignOp }
  <0>             @compoundAssignOp          { \inp len -> scannedToken inp $ CompoundAssignOp (extractTokenString inp len) }
  <0>             @incrementOp               { \inp len -> scannedToken inp $ IncrementOp (extractTokenString inp len) }
  <0>             $arithOp                   { \inp len -> scannedToken inp $ ArithmeticOp (extractTokenString inp len) }
  <0>             @relOp                     { \inp len -> scannedToken inp $ RelationOp (extractTokenString inp len) }
  <0>             @eqOp                      { \inp len -> scannedToken inp $ EquationOp (extractTokenString inp len) }
  <0>             @condOp                    { \inp len -> scannedToken inp $ ConditionOp (extractTokenString inp len) }
  <0>             \{                         { \inp len -> scannedToken inp LCurly }
  <0>             \}                         { \inp len -> scannedToken inp RCurly }
  <0>             \(                         { \inp len -> scannedToken inp LParen }
  <0>             \)                         { \inp len -> scannedToken inp RParen }
  <0>             \[                         { \inp len -> scannedToken inp LBrack }
  <0>             \]                         { \inp len -> scannedToken inp RBrack }
  <0>             \?                         { \inp len -> scannedToken inp Choice }
  <0>             \:                         { \inp len -> scannedToken inp Colon }
  <0>             \;                         { \inp len -> scannedToken inp Semicolon }
  <0>             \,                         { \inp len -> scannedToken inp Comma }
  <0>             \!                         { \inp len -> scannedToken inp Negate }
--  <0>             $invalid                   { scannedToken <**> (InvalidChar $ extractTokenString) }


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
           | IncrementOp String
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
           | EOF
           | InvalidChar String
           deriving (Eq)

instance Show Token where
  show (Keyword k) = k
  show (Identifier s) = "IDENTIFIER " ++ s
  show (CharLiteral s) = "CHARLITERAL " ++ s
  show (IntLiteral s) = "INTLITERAL " ++ s
  show (BooleanLiteral s) = "BOOLEANLITERAL " ++ s
  show (StringLiteral s) = "STRINGLITERAL " ++ s
  show AssignOp = "="
  show (IncrementOp s) = s
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
  show EOF = "EOF"
  show (InvalidChar s) = s

{-| Smart constructor to create a 'ScannedToken' by extracting the line and
column numbers from an 'AlexPosn'. -}
scannedToken :: AlexInput -> Token -> Alex ScannedToken
scannedToken ((AlexPn _ lineNo columnNo), _, _, _) tok = return (ScannedToken lineNo columnNo tok)

extractTokenString :: AlexInput -> Int -> String
extractTokenString ((AlexPn _ lineNo columnNo), _, _, str) len = take len str

(<**>) :: (a -> d -> c) -> (a -> b -> d) -> a -> b -> c
(f1 <**> f2) in1 in2 = f1 in1 $ f2 in1 $ in2

---------------------------- Helper functions for scanning -----------------------------

-- UserState to track comment depth and value of string literal
data AlexUserState = AlexUserState { lexerCommentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0 }

alexEOF :: Alex ScannedToken
alexEOF = Alex $ \s@AlexState{alex_pos=(AlexPn _ lineNo columnNo)} -> Right(s, ScannedToken lineNo columnNo EOF)

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth depth = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerCommentDepth=depth} }, ())

----- Special scanning functions ------

type Action = AlexInput -> Int -> Alex ScannedToken

enterComment :: Action
enterComment inp len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip inp len

exitComment :: Action
exitComment inp len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode 0)
       skip inp len


---------------------------- Scanning entry point -----------------------------

catchErrors :: Alex a -> Alex (a, Maybe String)
catchErrors (Alex al) = Alex (\s -> case al s of
                                      Right (s', x) -> Right (s', (x, Nothing))
                                      Left message -> Right (s, (undefined, Just message)))


scan :: String -> [Either String ScannedToken]
scan str =
    let loop =
            do (t, m) <- catchErrors alexMonadScan
               let tok@(ScannedToken line col raw) = t
               if (raw == EOF)
                   then do depth <- getLexerCommentDepth
                           if (isJust m)
                              then return [Left $ fromJust m]
                              else if (depth == 0)
                                   then return []
                                   else return [Left "comment not closed at EOF"]
                   else do toks <- loop
                           if (isJust m)
                              then return (Left (fromJust m) : toks)
                              else return (Right tok : toks)
    in case runAlex str loop of
         Left m -> [Left m]
         Right toks -> toks


formatTokenOrError :: Either String ScannedToken -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right tok) = Right $ unwords [ show $ line tok
                                                 , show $ extractRawToken tok
                                                 ]
}
