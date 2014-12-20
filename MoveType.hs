module MoveType where
import Prelude ()
import Data.Eq
import Data.Maybe
import Text.Show

import Square
import Piece


data MoveType = Moves | Captures deriving (Show, Eq)

data Move desc = 
    PawnMove desc (Maybe OfficerType) | 
    OfficerMove OfficerType desc


data Legal a = Legal a
