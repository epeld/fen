module PawnMovement where
import Control.Applicative

import Square
import Piece
import Directions
import SquareOffsets
import Movement
import MoveType


movementFn :: MoveType -> Color -> MovementFn
movementFn Captures color sq = shortRange (pawnAttackSources color) sq
movementFn Moves color sq = take 2 <$> longRange [pawnMoveSource color] sq


behind :: Square -> Color -> Maybe Square
behind sq = add sq . offset . pawnMoveSource
