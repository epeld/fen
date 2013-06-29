module PGNMoveEssentials where
import Square 
import Piece
import MoveType
import MovingPiece

data Essentials = Essentials {
    hint :: Maybe Hint,
    moveType :: MoveType,
    destination :: Square
    } deriving (Show, Eq)

data Hint = FileHint Char | RankHint Int | SquareHint Square deriving (Show, Eq)

compatible e mp = maybe True (flip compatible' mp) $ hint e 
compatible' (FileHint c) mp = file (MovingPiece.square mp) == c
compatible' (RankHint r) mp = rank (MovingPiece.square mp) == r
compatible' (SquareHint s) mp = MovingPiece.square mp == s
