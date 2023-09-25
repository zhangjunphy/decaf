module SourceLoc (Range (..), Posn (..), Located(..)) where

data Posn = Posn
  { offset :: !Int,
    row :: !Int,
    col :: !Int
  }
  deriving (Eq, Ord, Show)

data Range = Range
  { start :: Posn,
    stop :: Posn
  }
  deriving (Eq, Ord, Show)

data Located a = At Range a
  deriving (Show)
