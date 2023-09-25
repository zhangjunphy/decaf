{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module SourceLoc (Posn (..), Range (..), Located (..), unLocate) where

import Formatting (formatToString, int, shown, (%))
import Control.Monad.State

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

unLocate :: Located a -> a
unLocate (LocatedAt _ a) = a
