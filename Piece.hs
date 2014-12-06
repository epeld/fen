module Piece where
import Prelude ()
import Data.Eq
import Text.Show

data PieceType = PieceType deriving (Show, Eq)
data Piece = Piece PieceType Color deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)
