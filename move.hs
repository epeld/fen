module Move (Move(..), Move.position, Move.pieceType,
             Move.board, Move.square, whose, ) where

import Square (Square)
import MoveType (MoveType(..))
import Position (Position, Promotion, 
                 board, whoseTurn, )
import MovingPiece (MovingPiece, position, square, 
                    pieceType, isPawn)

data Move = Move { movingPiece :: MovingPiece, moveType :: MoveType,
                   destination :: Square,  promotion :: Maybe Promotion }

whose = Position.whoseTurn. Move.position
square = MovingPiece.square. Move.movingPiece
position = MovingPiece.position. Move.movingPiece
board = Position.board . Move.position
pieceType = MovingPiece.pieceType. Move.movingPiece
