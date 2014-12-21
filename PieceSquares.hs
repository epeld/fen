module PieceSquares where
import Prelude (Enum)
import Data.Function
import Data.List

import Square (Square, add)
import Piece
import Directions
import SquareOffsets
import Movement


officerSquares :: OfficerType -> Square -> [[Square]]
officerSquares officer sq = applyFn officer sq (directions officer)

-- Returns the squares that a pawn could stand TO MOVE TO sq
pawnMoveSquares :: Color -> Square -> [Square]
pawnMoveSquares c sq = concat $ applyN 2 sq [pawnMoveSource c]
    where n = if rank sq == firstRank c
              then 2 else 1

-- Returns the squares that a pawn could stand TO CAPTURE ON sq
pawnAttackSquares :: Color -> Square -> [Square]
pawnAttackSquares c sq = concat $ applyN 1 sq (pawnAttackSources c)
