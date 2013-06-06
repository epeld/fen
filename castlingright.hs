module CastlingRight ( Right(..), Side(..)) where
import Color (Color)
import CastlingSide (Side(..))

data Right = Castle Side Color deriving (Show, Eq)
