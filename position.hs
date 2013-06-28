module Position( Position(..), Position.readSquare, Promotion, 
                 friendlyColor, enemyColor, containsFriendlyPiece,
                 containsEnemyPiece, isPawnCapturableSquare, isEmpty,
                 Position.lastRank, isPassantSquare, enemy,
                 friendly, friendlySquares, enemySquares,) where
import Data.Char (toLower)

import Square (Square)
import Piece (Piece(Piece), OfficerType, color, PieceType)
import Color (Color, invert, lastRank, initialPawnRank)
import Board (Board, readSquare, pieceSquares, colorSquares,)
import CastlingRight (Right)

data Position = Position {
    board :: Board,
    whoseTurn :: Color,
    enPassant :: Maybe Square,
    castlingRights :: [Right],
    fullMoves :: Int,
    halfMoves :: Int
    } deriving (Show, Eq)

type Promotion = OfficerType

isEmpty p s = Position.readSquare p s == Nothing

isPawnCapturableSquare :: Position -> Square -> Bool
isPawnCapturableSquare p s = if isPassantSquare p s
    then True
    else containsEnemyPiece p s

isPassantSquare p s = enPassant p == Just s

containsPieceSatisfying criterion p s = maybe False criterion piece
    where piece = Position.readSquare p s

containsEnemyPiece p = containsPieceColored (enemyColor p) p
containsFriendlyPiece p = containsPieceColored (friendlyColor p) p

containsPieceColored c = containsPieceSatisfying (hasColor c)
    where hasColor c pc = color pc == c

readSquare p s = Board.readSquare (board p) s

friendlySquares p = Board.colorSquares (friendlyColor p) (board p)
enemySquares p = Board.colorSquares (enemyColor p) (board p)

enemy :: PieceType -> Position -> Square
enemy pt p = Position.findSquare pt (enemyColor p) p
friendly pt p = Position.findSquare pt (friendlyColor p) p

findSquare :: PieceType -> Color -> Position -> Square
findSquare pt = findSquare' . Piece pt
findSquare' pc p = case Board.pieceSquares pc $ board p of
    [] -> error $ "No such piece: " ++ pcs
    [x] -> x
    _  -> error $ "More than one such piece: " ++ pcs
    where pcs = map toLower (show pc)

enemyColor = invert . whoseTurn
friendlyColor = whoseTurn

lastRank :: Position -> Int
lastRank = Color.lastRank. whoseTurn

initialPawnRank = Color.initialPawnRank. whoseTurn
