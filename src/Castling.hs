module Castling where
import Prelude ()
import Data.Eq
import Data.Ord
import Text.Show

import Piece

data Side = Kingside | Queenside deriving (Show, Ord, Eq)
data CastlingRight = Castling Color Side deriving (Show, Ord, Eq)
