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

data CastlingRight = Castle Side Color deriving (Show, Eq)
data Side = Kinside | Queenside deriving (Show, Eq)
