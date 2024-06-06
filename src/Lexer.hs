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

-- Lexer entry point
module Lexer
  ( Token (..),
    Alex (..),
    scan,
    formatTokenOrError,
    alexMonadScan,
    runAlex,
  )
where

import Data.ByteString.Lazy (ByteString)
import Lexer.Lex
import Lexer.Token
import Util.SourceLoc qualified as SL

-- Produce error tokens for later use
catchErrors :: Alex (SL.Located Token) -> Alex (SL.Located Token)
catchErrors (Alex al) =
  Alex
    ( \s -> case al s of
        Right (s'@AlexState {alex_ust = ust}, t@(SL.LocatedAt _ EOF)) -> eofCheck s' t
        Right (s', x) -> Right (s', x)
        Left message -> Left message
    )
  where
    eofCheck s@AlexState {alex_ust = ust} tok@(SL.LocatedAt loc _) =
      case ust of
        val
          | lexerStringState ust -> Right (s, SL.LocatedAt loc $ Error "string not closed at EOF")
          | lexerCharState ust -> Right (s, SL.LocatedAt loc $ Error "char not closed at EOF")
          | lexerCommentDepth ust > 0 -> Right (s, SL.LocatedAt loc $ Error "comment not closed at EOF")
          | otherwise -> Right (s, tok)

scan :: ByteString -> [Either String (SL.Located Token)]
scan str =
  let loop = do
        tokOrError <- catchErrors alexMonadScan
        case tokOrError of
          t@(SL.LocatedAt _ (Error m)) ->
            do
              toks <- loop
              return (Right t : toks)
          t@(SL.LocatedAt _ tok) ->
            if tok == EOF
              then return []
              else do
                toks <- loop
                return (Right t : toks)
   in case runAlex str loop of
        Left m -> [Left m]
        Right toks -> toks

formatTokenOrError :: Either String (SL.Located Token) -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right (SL.LocatedAt (SL.Range (SL.Posn _ line _) _) tok)) =
  Right $ unwords [show line, show tok]
