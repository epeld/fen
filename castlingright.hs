module CastlingRight ( Right(..), Side(..), rightToChar, rightsToString) where
import Data.Char (toUpper, toLower)

import Color (Color(..))
import CastlingSide (Side(..))

data Right = Castles Side Color deriving (Show, Eq)

rightsToString = map rightToChar

rightToChar r@(Castles s c) | c == White = toUpper cr
                            | c == Black = toLower cr
    where cr = rightToLowerChar r

rightToLowerChar (Castles Kingside _) = 'k'
rightToLowerChar (Castles Queenside _) = 'q'
