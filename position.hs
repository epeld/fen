module Position(
    Position(..),
    Position.readSquare,
    Promotion,
    friendlyColor,
    enemyColor,
    ) where
import Square (Square)
import Piece (
    Piece,
    OfficerType,
    )
import Color (
    Color,
    invert,
    )
import Board (
    Board,
    readSquare,
    )
import CastlingRight (Right)

data Position = Position {
    board :: Board,
    whoseTurn :: Color,
    enPassant :: Maybe Square,
    castlingRights :: [Right],
    fullMoves :: Int,
    halfMoves :: Int
    } deriving Show

type Promotion = OfficerType

readSquare p s = Board.readSquare (board p) s

enemyColor = invert . whoseTurn
friendlyColor = whoseTurn
