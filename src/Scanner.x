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
{-# LANGUAGE DuplicateRecordFields #-}

module Scanner ( Token(..)
               , Alex(..)
               , AlexPosn(..)
               , scan
               , formatTokenOrError
               , alexMonadScan
               , runAlex
               , getLexerPosn
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
$syntaxChars = [\; \, \: \? \! \{\} \[\] \(\) \= \+ \- \* \/ \& \| \% $white2 \> \<]

-- invalid characters for better error handling
$invalid = $printable # [a-z A-Z 0-9 _ $syntaxChars \' \"]

-- rules
tokens :-
  <0>                     $white2+                   ;
  <0>                     "//".*                     ;
  <0, inComment>          "/*"                       { enterComment `andBegin` inComment }
  <inComment>             "*/"                       { exitComment }
  <inComment>             [.\n]                      ;
  <0>                     "\""                       { enterString `andBegin` inString }
  <inString>              "\\\\"                     { addToString '\\'}
  <inString>              "\\n"                      { addToString '\n'}
  <inString>              "\\t"                      { addToString '\t'}
  <inString>              "\\\'"                     { addToString '\''}
  <inString>              \\\"                       { addToString '"'}
  <inString>              [.\n]                      { addCurrentToString }
  <inString>              "\""                       { exitString }
  <0>                     "\'"                       { enterChar `andBegin` inChar }
  <inChar>                "\\\\"                     { addToChar '\\'}
  <inChar>                "\\n"                      { addToChar '\n'}
  <inChar>                "\\t"                      { addToChar '\t'}
  <inChar>                "\\\'"                     { addToChar '\''}
  <inChar>                \\\"                       { addToChar '"'}
  <inChar>                [.\n]                      { addCurrentToChar }
  <inChar>                "\'"                       { exitChar }
  <inString, inChar>      "\\"                       { scannerError $ \_ -> "invalid escape sequence" }
  <0>                     $syntaxChars ^ @keyword    { stringToken Keyword }
  <0>                     $syntaxChars ^ @id         { stringToken Identifier }
  <0>                     @intLiteral                { stringToken IntLiteral }
  <0>                     @boolLiteral               { stringToken BooleanLiteral }
  <0>                     @stringLiteral             { stringToken StringLiteral }
  <0>                     $assignOp                  { plainToken AssignOp }
  <0>                     @compoundAssignOp          { stringToken CompoundAssignOp }
  <0>                     @incrementOp               { stringToken IncrementOp }
  <0>                     $arithOp                   { stringToken ArithmeticOp }
  <0>                     @relOp                     { stringToken RelationOp }
  <0>                     @eqOp                      { stringToken EquationOp }
  <0>                     @condOp                    { stringToken ConditionOp }
  <0>                     \{                         { plainToken LCurly }
  <0>                     \}                         { plainToken RCurly }
  <0>                     \(                         { plainToken LParen }
  <0>                     \)                         { plainToken RParen }
  <0>                     \[                         { plainToken LBrack }
  <0>                     \]                         { plainToken RBrack }
  <0>                     \?                         { plainToken Choice }
  <0>                     \:                         { plainToken Colon }
  <0>                     \;                         { plainToken Semicolon }
  <0>                     \,                         { plainToken Comma }
  <0>                     \!                         { plainToken Negate }
  <0>                     $invalid                   { scannerError $ \s -> "invalid character: " ++ s }


----------------------------- Representing tokens -----------------------------

{
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
           | Error String
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
  show (Error s) = s

---------------------------- Helper functions for scanning -----------------------------

-- UserState to track comment depth and value of string literal
data AlexUserState = AlexUserState { lexerCommentDepth :: Int
                                   , lexerStringState :: Bool
                                   , lexerCharState :: Bool
                                   , lexerStringValue :: String
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0
                                  , lexerStringState = False
                                  , lexerCharState = False
                                  , lexerStringValue = ""
                                  }

alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{alex_pos=(AlexPn _ lineNo columnNo)} -> Right(s, EOF)

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth depth = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerCommentDepth=depth} }, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue value = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerStringValue=value} }, ())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState state = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerStringState=state} }, ())

getLexerCharState :: Alex Bool
getLexerCharState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCharState ust)

setLexerCharState :: Bool -> Alex ()
setLexerCharState state = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerCharState=state} }, ())

getLexerPosn :: Alex AlexPosn
getLexerPosn = Alex $ \s@AlexState{ alex_pos = pos } -> Right (s, pos)

----- Scanning functions ------

type Action = AlexInput -> Int -> Alex Token

plainToken :: Token -> Action
plainToken tok _ _ =
    return tok

stringToken :: (String -> Token) -> Action
stringToken tok (_, _, _, str) len =
    let tokenContent = take len str in
    return (tok tokenContent)

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

enterString :: Action
enterString inp len =
    do setLexerStringState True
       setLexerStringValue ""
       skip inp len

exitString :: Action
exitString ((AlexPn _ lineNo columnNo), _, _, _) len =
    do value <- getLexerStringValue
       setLexerStringState False
       alexSetStartCode 0
       return (StringLiteral $ reverse value)

addToString :: Char -> Action
addToString c inp len =
    do value <- getLexerStringValue
       setLexerStringValue (c : value)
       skip inp len

addCurrentToString :: Action
addCurrentToString inp@(_, _, _, str) len = addToString (head str) inp len

enterChar :: Action
enterChar inp len =
    do setLexerCharState True
       setLexerStringValue ""
       skip inp len

exitChar :: Action
exitChar ((AlexPn _ lineNo columnNo), _, _, _) len =
    do value <- getLexerStringValue
       setLexerCharState False
       alexSetStartCode 0
       return (CharLiteral value)

addToChar :: Char -> Action
addToChar c inp len =
    do value <- getLexerStringValue
       if (length value > 0)
          then scannerError (\_ -> "character literal not closed") inp len
          else do setLexerStringValue [c]
                  skip inp len

addCurrentToChar :: Action
addCurrentToChar inp@(_, _, _, str) len = addToChar (head str) inp len

scannerError :: (String -> String) -> Action
scannerError fn ((AlexPn _ lineNo columnNo), _, _, str) len =
    let content = take len str
    in return (Error $ fn content)


---------------------------- Scanner entry point -----------------------------

-- Produce error tokens for later use
catchErrors :: Alex Token -> Alex Token
catchErrors (Alex al) =
    Alex (\s -> case al s of
                  Right (s'@AlexState{alex_ust=ust}, EOF) -> eofCheck s' EOF
                  Right (s', x) -> Right (s', x)
                  Left message -> Left message)
    where eofCheck s@AlexState{alex_pos=(AlexPn _ lineNo colNo), alex_ust=ust} tok =
              let error message = Error message
              in case ust of
                   val | (lexerStringState ust) -> Right (s, error "string not closed at EOF")
                       | (lexerCharState ust) -> Right (s, error "char not closed at EOF")
                       | (lexerCommentDepth ust > 0) -> Right (s, error "comment not closed at EOF")
                       | otherwise -> Right(s, tok)

scan :: String -> [(Either String (AlexPosn, Token))]
scan str =
    let loop = do
          pos <- getLexerPosn
          tokOrError <- catchErrors alexMonadScan
          case tokOrError of
              Error m -> alexError m
              tok ->
                  if (tok == EOF)
                  then return []
                  else do toks <- loop
                          return ((Right (pos, tok)) : toks)
    in case runAlex str loop of
         Left m -> [Left m]
         Right toks -> toks

formatTokenOrError :: Either String (AlexPosn, Token) -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right ((AlexPn _ line col), tok))
    = Right $ unwords [ show $ line
                      , show $ tok
                      ]
}
