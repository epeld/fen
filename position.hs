module Position( Position(..), Position.readSquare, Promotion, 
                 friendlyColor, enemyColor, containsFriendlyPiece,
                 containsEnemyPiece, isCapturableSquare) where
import Square (Square)
import Piece ( Piece, OfficerType, color,)
import Color ( Color, invert,)
import Board ( Board, readSquare,)
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

isCapturableSquare :: Position -> Square -> Bool
isCapturableSquare p s = if isPassantSquare p s
    then True
    else containsEnemyPiece p s

isPassantSquare p s = enPassant p == Just s

containsPieceSatisfying criterion p s = maybe False criterion piece
    where piece = Position.readSquare p s

containsEnemyPiece p = containsPieceSatisfying (hasEnemyColor p) p
containsFriendlyPiece p = containsPieceSatisfying (hasFriendlyColor p) p

readSquare p s = Board.readSquare (board p) s

hasEnemyColor :: Position -> Piece -> Bool
hasEnemyColor p pc = color pc == enemyColor p

hasFriendlyColor :: Position -> Piece -> Bool
hasFriendlyColor p = not. hasEnemyColor p

enemyColor = invert . whoseTurn
friendlyColor = whoseTurn
