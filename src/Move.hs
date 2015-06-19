module Move where
import Square
import Piece

import Prelude ()
import Data.Eq
import Data.Maybe
import Text.Show

data Move desc = 
    PawnMove {
        description :: desc,
        promotion :: Maybe OfficerType } | 
    OfficerMove {
        officerType :: OfficerType,
        description :: desc }
