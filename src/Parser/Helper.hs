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

module Parser.Helper where

import Data.Text (Text)
import Lexer (Alex (..), Token (..), alexMonadScan)
import Util.SourceLoc as SL
import Text.Printf (printf)

getID :: Token -> Text
getID (Identifier id) = id

getLiteral :: Token -> Text
getLiteral (IntLiteral i) = i
getLiteral (BooleanLiteral b) = b
getLiteral (CharLiteral c) = c
getLiteral (StringLiteral s) = s

getOp :: Token -> Text
getOp (IncrementOp op) = op
getOp (CompoundAssignOp op) = op

unionOf :: SL.Located a -> SL.Located b -> Range
unionOf (SL.LocatedAt loc1 _) (SL.LocatedAt loc2 _) = combineRanges loc1 loc2
  where
    combineRanges (SL.Range start1 stop1) (SL.Range start2 stop2) =
      let start = if (SL.offset start1) < (SL.offset start2) then start1 else start2
          stop = if (SL.offset stop1) > (SL.offset stop2) then stop1 else stop2
       in SL.Range start stop

lexerwrap :: (SL.Located Token -> Alex a) -> Alex a
lexerwrap s = do
  token <- alexMonadScan
  s token

parseError :: SL.Located Token -> Alex a
parseError (SL.LocatedAt (SL.Range (SL.Posn _ row col) _) tok) = do
  Alex $ \_ -> Left $ printf "%d:%d: Error handling token '%s'" row col (show tok)
