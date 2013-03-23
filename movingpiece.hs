module MovingPiece where
import Square
import Internals

data MovingPiece = PieceFromPosition {
    position :: LegalPosition,
    square::Square
    } deriving (Show)
