module CastlingRight (
    Right(..),
    Side(..)
    ) where
import Color (Color)

data Right = Castle Side Color deriving (Show, Eq)
data Side = Kinside | Queenside deriving (Show, Eq)
