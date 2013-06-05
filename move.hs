module Move (Move, move, Move.position, isPawnMove, moveType,
             Move.movingPiece, destination, promotion, Move.board,
             Move.square, isPassantMove, isTwoStepPawnMove,
             whose, ) where

import Prelude hiding (elem)
import Data.Maybe (maybe, fromJust)
import Control.Monad (when, unless,)
import Control.Monad.Error (throwError)

import Square (Square, rank, twice, up)
import Piece (PieceType(..), OfficerType,)
import Color (Color(..))
import MoveType (MoveType(..))
import Position (Position, readSquare, Promotion, lastRank, isPassantSquare,
                 board, whoseTurn, board,)
import qualified ProjectedRange ( inferMoveType)
import ErrorMonad (ErrorMonad, 
                   Reason(NoPromotion, LastRankPromote, NotInRange),)
import MovingPiece (MovingPiece, position, square, color, 
                    pieceType, movingPiece, isPawn)

data Move = Move { movingPiece :: MovingPiece, moveType :: MoveType,
                   destination :: Square,  promotion :: Maybe Promotion }

whose = Position.whoseTurn. Move.position
square = MovingPiece.square. Move.movingPiece
position = MovingPiece.position. Move.movingPiece
board = Position.board . Move.position
pieceType = MovingPiece.pieceType. Move.movingPiece
isPawnMove = isPawn. Move.movingPiece
