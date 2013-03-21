module Internals where
import Square
import Piece

data LegalPosition = LegalPosition deriving Show

data MoveErrors = LastRankPromote | NoPieceToMove | NoPromotion | ColorsMismatch
raiseError _ = Nothing

type Promotion = OfficerType
data MovingPiece = PieceFromPosition {
    getPosition::LegalPosition, getSquare::Square
    } deriving (Show)
data Move = Move MovingPiece Square (Maybe Promotion)
data ClassifiedMove = Standard Move | Capturing Move
