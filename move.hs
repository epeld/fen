module Move (Move(..), Move.position, Move.pieceType,
             Move.board, Move.square, whose,
             isPawnMove, isPassantMove, isTwoStepPawnMove,
             isLastRankMove,) where

import Square (Square, rank, twice, up)
import MoveType (MoveType(..))
import Position (Position, Promotion, lastRank, isPassantSquare,
                 board, whoseTurn, )
import qualified MovingPiece (MovingPiece, position, square, 
                              pieceType, isPawn, color, )

data Move = Move { movingPiece :: MovingPiece.MovingPiece, moveType :: MoveType,
                   destination :: Square,  promotion :: Maybe Promotion }

whose = Position.whoseTurn. Move.position
square = MovingPiece.square. Move.movingPiece
position = MovingPiece.position. Move.movingPiece
board = Position.board . Move.position
pieceType = MovingPiece.pieceType. Move.movingPiece

isPawnMove = MovingPiece.isPawn. movingPiece

isLastRankMove :: Move -> Bool
isLastRankMove mv = rank (destination mv) == Position.lastRank p
    where p = position mv

isTwoStepPawnMove :: Move -> Bool
isTwoStepPawnMove mv = twice up (square mv) == Just dest && isPawnMove mv
    where dest = destination mv

isPassantMove mv = isPassantSquare (position mv) (destination mv)
