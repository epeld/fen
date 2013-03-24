module Internals where

import Board
import Square
import Piece

import Control.Monad.Error 

data Reason = LastRankPromote | NoPieceToMove | NoPromotion | ColorsMismatch

instance Error Reason where
noMsg = error "noMsg called"
strMsg s = "strMsg called"

type ErrorMonad = Either Reason

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
data MoveType = Takes | Moves

readSquare p s = Board.readSquare (board p) s
