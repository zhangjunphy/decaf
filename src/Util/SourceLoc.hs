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

module Util.SourceLoc (Posn (..), Range (..), Located (..), unLoc, getLoc) where

import Formatting (formatToString, int, shown, (%))

data Posn = Posn
  { offset :: !Int,
    row :: !Int,
    col :: !Int
  }
  deriving (Eq, Ord)

posn0 :: Posn
posn0 = Posn 0 0 0

instance Show Posn where
  show (Posn _ row col) = formatToString ("(" % int % ":" % int % ")") row col

data Range = Range
  { start :: Posn,
    stop :: Posn
  }
  deriving (Eq, Ord)

range0 :: Range
range0 = Range posn0 posn0

instance Show Range where
  show (Range start stop) = formatToString ("[" % shown % "-" % shown % "]") start stop


data Located a = LocatedAt Range a
  deriving (Show, Functor)

unLoc :: Located a -> a
unLoc (LocatedAt _ a) = a

getLoc :: Located a -> Range
getLoc (LocatedAt r _) = r
