module MovingPiece (MovingPiece(..), movingPiece, piece, 
                    pieceType, color, officerType, isPawn)where
import Data.Maybe (fromJust)
import Control.Monad.Error (throwError)

import Square (Square)
import qualified Piece ( Piece, pieceType, officerType, color, verifyHasColor,)
import Color (Color)
import Position (Position, whoseTurn, readSquare, board,
                 friendlyColor, enemyColor)
import ErrorMonad (ErrorMonad, Reason(NoPieceToMove),)
import MoveType(MoveType)
import Piece (PieceType(Pawn))

data MovingPiece = PieceFromPosition { position :: Position, square::Square }
                                       deriving (Show, Eq)

movingPiece :: Position -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (Piece.verifyHasColor c) x
    return (PieceFromPosition p s)

piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
color = Piece.color. piece
pieceType = Piece.pieceType. piece
officerType = Piece.officerType. pieceType

isPawn :: MovingPiece -> Bool
isPawn mp = pieceType mp == Pawn
