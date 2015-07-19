module Move where
import Prelude ()
import Data.Eq
import Data.Maybe
import Text.Show

import Square
import Piece
import MoveType


data Move src = 
    PawnMove { description :: Description src, promotion :: Maybe OfficerType } | 
    OfficerMove { description :: Description src, officerType :: OfficerType }


data Description src = Description { source :: src, destination :: Square, moveType :: MoveType }
