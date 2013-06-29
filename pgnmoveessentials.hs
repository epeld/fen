module PGNMoveEssentials where
import Square 
import Piece
import MoveType

data Essentials = Essentials {
    hint :: Maybe Hint,
    moveType :: MoveType,
    destination :: Square
    } deriving (Show, Eq)

data Hint = FileHint Char | RankHint Int | SquareHint Square deriving (Show, Eq)
