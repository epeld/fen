module MovingPiece where
import Square
import Internals
import Piece (
    Piece,
    Color(..),
    color
    )

import Data.Maybe (fromJust)
import Control.Monad.Error (throwError)

data MovingPiece = PieceFromPosition {
    position :: LegalPosition,
    square::Square
    } deriving (Show)

movingPiece :: LegalPosition -> Square -> ErrorMonad MovingPiece
movingPiece p s = do
    let c = whoseTurn p
    let x = p `readSquare` s
    maybe (throwError NoPieceToMove) (verifyHasColor c) x
    return (PieceFromPosition p s)

verifyHasColor :: Color -> Piece -> ErrorMonad ()
verifyHasColor c p = verifyColorsMatch c (color p)
verifyColorsMatch White White = return ()
verifyColorsMatch Black Black = return ()
verifyColorsMatch _ _ = throwError ColorsMismatch

piece mp = fromJust $ position mp `readSquare` MovingPiece.square mp
