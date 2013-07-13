module Move (Move(..), position, pieceType, enemyColor,
             board, square, whose,
             isPawnMove, isPassantMove, isTwoStepPawnMove,
             isLastRankMove,) where

import Color (invert)
import PawnRange (pawnDirection)
import Square (Square, rank, twice, up)
import MoveType (MoveType(..))
import qualified Position (Position, Promotion, lastRank, isPassantSquare,
                           board, whoseTurn, )
import qualified MovingPiece (MovingPiece, position, square, 
                              pieceType, isPawn, color, )

data Move = Move { 
    movingPiece :: MovingPiece.MovingPiece,
    moveType :: MoveType,
    destination :: Square,  
    promotion :: Maybe Position.Promotion } deriving (Show, Eq)

enemyColor = invert. whose
whose = Position.whoseTurn. position
square = MovingPiece.square. movingPiece
position = MovingPiece.position. movingPiece
board = Position.board . position
pieceType = MovingPiece.pieceType. movingPiece

isPawnMove = MovingPiece.isPawn. movingPiece

isLastRankMove :: Move -> Bool
isLastRankMove mv = rank (destination mv) == Position.lastRank p
    where p = position mv

-- TODO unit test for both black and white
isTwoStepPawnMove :: Move -> Bool
isTwoStepPawnMove mv = twice forward (square mv) == Just dest && isPawnMove mv
    where dest = destination mv
          forward = pawnDirection (whose mv)

isPassantMove mv = Position.isPassantSquare (position mv) (destination mv)
