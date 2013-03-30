module MovingPiece (
    MovingPiece(..),
    movingPiece,
    piece,
    pieceType,
    color,
    MovingPiece.range,
    )where
import Data.Maybe (fromJust)
import Control.Monad.Error (throwError)

import Square (Square)
import qualified Piece (
    Piece,
    pieceType,
    color,
    verifyHasColor,
    )
import Color (Color)
import Position (
    Position,
    whoseTurn,
    readSquare,
    )
import ErrorMonad (
    ErrorMonad,
    Reason(NoPieceToMove),
    )
import Range(
    Range,
    range,
    )
import MoveType(MoveType)

data MovingPiece = PieceFromPosition {
    position :: Position,
    square::Square
    } deriving (Show)

movingPiece :: Position -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (Piece.verifyHasColor c) x
    return (PieceFromPosition p s)

piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
color = Piece.color . piece
pieceType = Piece.pieceType . piece

range :: MovingPiece -> MoveType -> Range
range mp = Range.range 
    (color mp)
    (pieceType mp)
    (square mp)
