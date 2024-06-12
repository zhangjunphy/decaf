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
    AlexState (..),
    AlexUserState(..),
    scan,
    alexMonadScan,
    runAlex,
    addError,
    alexError,
    getAlexState
  )
where

import Data.ByteString.Lazy (ByteString)
import Lexer.Lex
import Lexer.Token
import Util.SourceLoc qualified as SL
import Types (CompileError(..))
import Data.Text qualified as Text

-- Produce error tokens for later use
catchErrors :: SL.Located Token -> Alex ()
catchErrors tok = do
  s <- getAlexState
  case tok of 
    t@(SL.LocatedAt _ EOF) -> eofCheck s t
    t -> return ()
  where
    eofCheck s@AlexState {alex_ust = ust} tok@(SL.LocatedAt loc _) =
      case ust of
        val
          | lexerStringState ust -> addError $ CompileError (Just loc) "String not closed at EOF"
          | lexerCharState ust -> addError $ CompileError (Just loc) "Char not closed at EOF"
          | lexerCommentDepth ust > 0 -> addError $ CompileError (Just loc) "Comment not closed at EOF"
          | otherwise -> return ()

scan :: ByteString -> Either [CompileError] [SL.Located Token]
scan str =
  let loop = do
        tok'@(SL.LocatedAt _ tok) <- alexMonadScan
        catchErrors tok'
        if tok == EOF
          then return []
          else do
            toks <- loop
            return (tok' : toks)
      tokenAndErrors = loop >>= \tokens -> do
        state <- getAlexState
        let errs = errors $ alex_ust state
        return (errs, tokens)
   in case runAlex str tokenAndErrors of
        Left m -> Left [CompileError Nothing $ Text.pack m]
        Right (errs, _) | not $ null errs -> Left errs
        Right (_, toks) -> Right toks
