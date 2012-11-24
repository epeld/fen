module Pgn where

import Square
import Game
import Chess
import Piece

data PGNMove = PawnMove (Maybe Hint) MoveType Square (Maybe OfficerType) |
               PieceMove PieceType (Maybe Hint) MoveType Square |
               Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving Show
