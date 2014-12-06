module MoveTypes where
import Prelude ()
import Data.Eq
import Text.Show

import Square
import Piece

data Move = Move deriving (Show, Eq)

data MoveError = Ambiguous [SpecifiedMove] | Invalid deriving (Show, Eq)

-- Specified Move: guarantees that there is a piece at its source square
data SpecifiedMove = Specified Move Square deriving (Show, Eq)

-- LegalMove: guarantees that the move is legal and results in a legal position
data Legal a = Legal a
type LegalMove = Legal SpecifiedMove

-- SpecifiedPawnMove: guarantees that the moved piece is a pawn
newtype SpecifiedPawnMove = PawnMove SpecifiedMove

-- SpecifiedOfficerMove: guarantees that the moved piece is an officer
newtype SpecifiedOfficerMove = OfficerMove SpecifiedMove


-- TODO move:

