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

data Range = Range {
    movingPiece :: MovingPiece,
    moveType :: MoveType
    }

series :: Range -> [SquareSeries]
series r = fromMaybeSquares $ seriesM r

fromMaybeSquares msqs = map fromJust <$> validSquares
    where validSquares = takeUntilInvalid <$> msqs

takeUntilInvalid = takeWhile isJust

seriesM :: Range -> [[Maybe Square]]
seriesM r | isPawn = pawnSeriesM mp mt
          | otherwise = officerSeriesM mp mt
    where isPawn = pieceType mp == Pawn
          mp = movingPiece r
          mt = moveType r
