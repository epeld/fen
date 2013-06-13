module Range ( series, Range, movingPiece, moveType, 
               range, Range.position, Range.pieceType, Range.isPawn) where

import Data.Maybe (fromJust, isJust,)
import Control.Monad ((>=>), )
import Control.Applicative ((<*>), (<$>))

import PawnRange( pawnSeriesM)
import OfficerRange( officerSeriesM)
import MoveType ( MoveType (Takes, Moves))
import MovingPiece ( MovingPiece, pieceType, position, isPawn)
import Square ( Square, Series,)
import Piece ( PieceType(Pawn),)

data Range = Range { movingPiece :: MovingPiece, moveType :: MoveType }

range = Range

series :: Range -> [Series]
series r = fromMaybeSquares $ seriesM r

fromMaybeSquares msqs = map fromJust <$> validSquares
    where validSquares = takeUntilInvalid <$> msqs

takeUntilInvalid = takeWhile isJust

seriesM :: Range -> [[Maybe Square]]
seriesM r | pieceIsPawn = pawnSeriesM mp mt
          | otherwise = officerSeriesM mp mt
    where 
          pieceIsPawn = MovingPiece.isPawn mp
          mp = movingPiece r
          mt = moveType r

position = MovingPiece.position. movingPiece
pieceType = MovingPiece.pieceType. movingPiece
isPawn = MovingPiece.isPawn. movingPiece
