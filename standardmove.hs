module StandardMove (StandardMove(..), position, pieceType,
                     board, square, whose,
                     isPawnMove, isPassantMove, isTwoStepPawnMove,
                     isLastRankMove,) where

import Square (Square, rank, twice, up)
import MoveType (MoveType(..))
import qualified Position (Position, Promotion, lastRank, isPassantSquare,
                           board, whoseTurn, )
import qualified MovingPiece (MovingPiece, position, square, 
                              pieceType, isPawn, color, )

data StandardMove = StandardMove { 
    movingPiece :: MovingPiece.MovingPiece,
    moveType :: MoveType,
    destination :: Square,  
    promotion :: Maybe Position.Promotion }

whose = Position.whoseTurn. position
square = MovingPiece.square. movingPiece
position = MovingPiece.position. movingPiece
board = Position.board . position
pieceType = MovingPiece.pieceType. movingPiece

isPawnMove = MovingPiece.isPawn. movingPiece

isLastRankMove :: StandardMove -> Bool
isLastRankMove mv = rank (destination mv) == Position.lastRank p
    where p = position mv

isTwoStepPawnMove :: StandardMove -> Bool
isTwoStepPawnMove mv = twice up (square mv) == Just dest && isPawnMove mv
    where dest = destination mv

isPassantMove mv = Position.isPassantSquare (position mv) (destination mv)
