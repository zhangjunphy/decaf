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


-- rules
tokens :-
  <0>             $white2+                   ;
  <0>             "//".*                     ;
  <0, inComment>  "/*"                       { enterComment `andBegin` inComment }
  <inComment>     [.\n]                      ;
  <inComment>     "*/"                       { exitComment }
  <0>             $syntaxChars ^ @keyword    { \input len -> scannedToken input $ Keyword (extractTokenString input len) }
  <0>             @charLiteral               { \input len -> scannedToken input $ CharLiteral (extractTokenString input len) }
  <0>             @intLiteral                { \input len -> scannedToken input $ IntLiteral (extractTokenString input len) }
  <0>             @boolLiteral               { \input len -> scannedToken input $ BooleanLiteral (extractTokenString input len) }
  <0>             @stringLiteral             { \input len -> scannedToken input $ StringLiteral (extractTokenString input len) }
  <0>             $syntaxChars ^ @id         { \input len -> scannedToken input $ Identifier (extractTokenString input len) }
  <0>             $assignOp                  { \input len -> scannedToken input AssignOp }
  <0>             @compoundAssignOp          { \input len -> scannedToken input $ CompoundAssignOp (extractTokenString input len) }
  <0>             @incrementOp               { \input len -> scannedToken input $ IncrementOp (extractTokenString input len) }
  <0>             $arithOp                   { \input len -> scannedToken input $ ArithmeticOp (extractTokenString input len) }
  <0>             @relOp                     { \input len -> scannedToken input $ RelationOp (extractTokenString input len) }
  <0>             @eqOp                      { \input len -> scannedToken input $ EquationOp (extractTokenString input len) }
  <0>             @condOp                    { \input len -> scannedToken input $ ConditionOp (extractTokenString input len) }
  <0>             \{                         { \input len -> scannedToken input LCurly }
  <0>             \}                         { \input len -> scannedToken input RCurly }
  <0>             \(                         { \input len -> scannedToken input LParen }
  <0>             \)                         { \input len -> scannedToken input RParen }
  <0>             \[                         { \input len -> scannedToken input LBrack }
  <0>             \]                         { \input len -> scannedToken input RBrack }
  <0>             \?                         { \input len -> scannedToken input Choice }
  <0>             \:                         { \input len -> scannedToken input Colon }
  <0>             \;                         { \input len -> scannedToken input Semicolon }
  <0>             \,                         { \input len -> scannedToken input Comma }
  <0>             \!                         { \input len -> scannedToken input Negate }


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

{-| Smart constructor to create a 'ScannedToken' by extracting the line and
column numbers from an 'AlexPosn'. -}
scannedToken :: AlexInput -> Token -> Alex ScannedToken
scannedToken ((AlexPn _ lineNo columnNo), _, _, _) tok = return (ScannedToken lineNo columnNo tok)

extractTokenString :: AlexInput -> Int -> String
extractTokenString ((AlexPn _ lineNo columnNo), _, _, str) len = take len str

---------------------------- Scanning entry point -----------------------------

-- UserState to track comment depth
data AlexUserState = AlexUserState { lexerCommentDepth :: Int
                                   , lexerStringValue :: String
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0, lexerStringValue = "" }

alexEOF :: Alex ScannedToken
alexEOF = return $ ScannedToken 0 0 EOF

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth depth = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=depth}}, ())

-- Actions
type Action = AlexInput -> Int -> Alex ScannedToken

enterComment :: Action
enterComment input len = do cd <- getLexerCommentDepth
                            setLexerCommentDepth (cd + 1)
                            skip input len

exitComment :: Action
exitComment input len = do cd <- getLexerCommentDepth
                           setLexerCommentDepth (cd - 1)
                           when (cd == 1) (alexSetStartCode 0)
                           skip input len

-- fill out this function with extra cases if you use error tokens
-- and want them to be treated as errors instead of valid tokens
catchErrors :: Alex a -> Alex (a, Maybe String)
catchErrors (Alex al) = Alex (\s -> case al s of
                                      Right (s', x) -> Right (s', (x, Nothing))
                                      Left message -> Right (s, (undefined, Just message)))

scan :: String -> [Either String ScannedToken]
scan str = let loop =  do (t, m) <- catchErrors alexMonadScan
                          let tok@(ScannedToken line col raw) = t
                          if (raw == EOF)
                              then do depth <- getLexerCommentDepth
                                      if (isJust m)
                                         then return [Left $ fromJust m]
                                         else if (depth == 0)
                                              then return []
                                              else return []
                              else do toks <- loop
                                      if (isJust m)
                                         then return (Left (fromJust m) : toks)
                                         else return (Right tok : toks)
               in case runAlex str loop of
                    Left m -> []
                    Right toks -> toks



formatTokenOrError :: Either String ScannedToken -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right tok) = Right $ unwords [ show $ line tok
                                                 , show $ extractRawToken tok
                                                 ]
}
