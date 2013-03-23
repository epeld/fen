module Internals where

import Board
import Square
import Piece

data LegalPosition = Position {
    board :: Board,
    whoseTurn :: Color,
    enPassant :: Maybe Square,
    castlingRights :: [CastlingRight],
    fullMove :: Int,
    halfMoves :: Int
    } deriving Show

type Promotion = OfficerType
data MovingPiece = PieceFromPosition {
    getPosition::LegalPosition, getSquare::Square
    } deriving (Show)
data Move = Move MovingPiece Square (Maybe Promotion)
data ClassifiedMove = Standard Move | Capturing Move

data CastlingRight = Castle Side Color deriving (Show, Eq)
data Side = Kinside | Queenside deriving (Show, Eq)
