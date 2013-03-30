module MovingPiece (
    MovingPiece(..),
    movingPiece,
    piece,
    )where
import Data.Maybe (fromJust)
import Control.Monad.Error (throwError)

import Square (Square)
import Piece (
    Piece,
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

data MovingPiece = PieceFromPosition {
    position :: Position,
    square::Square
    } deriving (Show)

movingPiece :: Position -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (verifyHasColor c) x
    return (PieceFromPosition p s)

piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
