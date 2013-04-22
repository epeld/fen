module Range ( series,) where

import Data.Maybe (isNothing, fromJust, isJust)
import Control.Monad ((>=>), liftM)
import Control.Applicative ((<*>), (<$>))

import PawnRange( pawnSeriesM)
import OfficerRange( officerSeriesM)
import MoveType ( MoveType (Takes, Moves))
import MovingPiece ( MovingPiece, pieceType,)
import Square ( Square, SquareSeries,)
import Piece ( PieceType(..),)

series :: MovingPiece -> MoveType -> [SquareSeries]
series mp mt = map fromJust <$> validSquares
    where validSquares = takeUntilInvalid <$> seriesM mp mt
          validSquares :: [[Maybe Square]]

takeUntilInvalid = takeWhile isJust

seriesM :: MovingPiece -> MoveType -> [[Maybe Square]]
seriesM mp mt | isPawn = pawnSeriesM mp mt
              | otherwise = officerSeriesM mp mt
    where isPawn = pieceType mp == Pawn
