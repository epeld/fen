module Internals where
import Square
import Piece

data LegalPosition = LegalPosition deriving Show

type Promotion = OfficerType
data MovingPiece = PieceFromPosition {
    getPosition::LegalPosition, getSquare::Square
    } deriving (Show)
data Move = Move MovingPiece Square (Maybe Promotion)
data ClassifiedMove = Standard Move | Capturing Move
