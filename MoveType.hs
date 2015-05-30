module MoveType where
import Prelude ()
import Data.Eq
import Data.Maybe
import Text.Show

import Square
import Piece
import Move


data MoveType = Moves | Captures deriving (Show, Eq)

pieceType :: Move a -> PieceType
pieceType (PawnMove a _) = Pawn
pieceType (OfficerMove ot _) = Officer ot
