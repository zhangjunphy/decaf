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
module Lexer.Lex ( Alex(..)
                 , alexMonadScan
                 , runAlex
                 , AlexState(..)
                 , AlexUserState(..)
                 ) where

import qualified Util.SourceLoc as SL

import Lexer.Token

import Data.Maybe
import Data.List.Split
import Data.Word (Word8)
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc
}

%wrapper "monadUserState-bytestring"


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
@intLiteral = @decimalLiteral | @hexLiteral
-- @charLiteral = \'@char\'
-- @stringLiteral = \"@char*\"
-- @literal = @intLiteral | @charLiteral | @boolLiteral

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
  <0>                     \"                         { enterString `andBegin` inString }
  <inString>              "\\"                       { addToString '\\'}
  <inString>              "\n"                       { addToString '\n'}
  <inString>              "\t"                       { addToString '\t'}
  <inString>              "\'"                       { addToString '\''}
  <inString>              \\\"                       { addToString '"'}
  <inString>              \"                         { exitString }
  <inString>              [.\n]                      { addCurrentToString }
  <0>                     "'"                        { enterChar `andBegin` inChar }
  <inChar>                "\\"                       { addToChar '\\'}
  <inChar>                "\n"                       { addToChar '\n'}
  <inChar>                "\t"                       { addToChar '\t'}
  <inChar>                "\'"                       { addToChar '\''}
  <inChar>                \\\"                       { addToChar '"'}
  <inChar>                "'"                        { exitChar }
  <inChar>                [.]                        { addCurrentToChar }
  <inString, inChar>      "\"                        { scannerError $ \_ -> "invalid escape sequence" }
  <0>                     @intLiteral                { stringToken IntLiteral }
  <0>                     @boolLiteral               { stringToken BooleanLiteral }
  <0>                     $syntaxChars ^ @keyword    { stringToken Keyword }
  <0>                     $syntaxChars ^ @id         { stringToken Identifier }
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
  <0>                     $invalid                   { scannerError $ \s -> BS.concat ["invalid character: ", s] }


{

---------------------------- Alex interface -----------------------------
-- The below code will be generated by the alex wrapper. They are put here for references.
{-
data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number

type AlexInput = (AlexPosn,   -- current position,
                  Char,       -- previous char
                  ByteString.ByteString -- current input string
                  Int64)      -- bytes consumed so far

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: ByteString.ByteString, -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int,       -- the current startcode
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }

runAlex          :: ByteString.ByteString -> Alex a -> Either String a

alexError        :: String -> Alex a

alexMonadScan    :: Alex a

-- token :: (ByteString.ByteString -> Int -> token) -> AlexAction token
-}


---------------------------- Helper functions for scanning -----------------------------

-- UserState to track comment depth and value of string literal
data AlexUserState = AlexUserState { lexerCommentDepth :: Int
                                   , lexerStringState :: Bool
                                   , lexerCharState :: Bool
                                   , lexerStringValue :: ByteString
                                   , inputLines :: [ByteString]
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0
                                  , lexerStringState = False
                                  , lexerCharState = False
                                  , lexerStringValue = ""
                                  , inputLines = []
                                  }

posnFromAlex :: AlexPosn -> SL.Posn
posnFromAlex (AlexPn offset row col) = SL.Posn offset (row-1) (col-1)

locatedAt :: AlexPosn -> AlexPosn -> a -> SL.Located a
locatedAt start stop = SL.LocatedAt (SL.Range (posnFromAlex start) (posnFromAlex stop))

alexEOF :: Alex (SL.Located Token)
alexEOF = Alex $ \s@AlexState{alex_pos=pos} -> Right(s, locatedAt pos pos EOF)

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth depth = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerCommentDepth=depth} }, ())

getLexerStringValue :: Alex ByteString
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: ByteString -> Alex ()
setLexerStringValue value = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerStringValue=value} }, ())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState state = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerStringState=state} }, ())

getLexerCharState :: Alex Bool
getLexerCharState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCharState ust)

setLexerCharState :: Bool -> Alex ()
setLexerCharState state = Alex $ \s -> Right (s { alex_ust=(alex_ust s) {lexerCharState=state} }, ())

getTokenStop :: AlexPosn -> ByteString -> AlexPosn
getTokenStop posn inp = Text.foldl' alexMove posn $ Enc.decodeUtf8 $ BS.toStrict inp

----- Scanning functions ------

type Action = AlexInput -> Int64 -> Alex (SL.Located Token)

stringToken :: (Text -> Token) -> Action
stringToken tok (start, _, inp, _) len =
  let content = BS.take len inp
      stop = getTokenStop start content
  in return $ locatedAt start stop $ tok $ Enc.decodeUtf8 $ BS.toStrict content

plainToken :: Token -> Action
plainToken tok (start, _, inp, _) len =
  let stop = getTokenStop start $ BS.take len inp
  in return $ locatedAt start stop tok

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
exitString (pos, _, _, _) len =
    do value <- getLexerStringValue
       setLexerStringState False
       alexSetStartCode 0
       return $ locatedAt pos pos (StringLiteral $ Enc.decodeUtf8 $ BS.toStrict $ BS.reverse value)

addToString :: Char -> Action
addToString c inp len =
    do value <- getLexerStringValue
       setLexerStringValue $ C8.cons c value
       skip inp len

addCurrentToString :: Action
addCurrentToString inp@(_, _, str, _) len = addToString (C8.head str) inp len

enterChar :: Action
enterChar inp len =
    do setLexerCharState True
       setLexerStringValue ""
       skip inp len

exitChar :: Action
exitChar (pos, _, _, _) len =
    do value <- getLexerStringValue
       setLexerCharState False
       alexSetStartCode 0
       return $ locatedAt pos pos (CharLiteral $ Enc.decodeUtf8 $ BS.toStrict value)

addToChar :: Char -> Action
addToChar c inp len =
    do value <- getLexerStringValue
       if (BS.length value > 0)
          then scannerError (\_ -> "character literal not closed") inp len
          else do setLexerStringValue $ C8.pack [c]
                  skip inp len

addCurrentToChar :: Action
addCurrentToChar inp@(_, _, str, _) len = addToChar (C8.head str) inp len

scannerError :: (ByteString -> ByteString) -> Action
scannerError fn (start, _, inp, _) len =
    let content = BS.take len inp
        stop = getTokenStop start content
    in return $ locatedAt start stop (Error $ Enc.decodeUtf8 $ BS.toStrict $ fn content)

}
