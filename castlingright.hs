module CastlingRight ( Right(..), Side(..)) where
import Color (Color)
import CastlingSide (Side(..))

data Right = Castles Side Color deriving (Show, Eq)
