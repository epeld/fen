{-#LANGUAGE NoMonomorphismRestriction #-}
module Pgn where

import Square
import Game
import Chess
import Piece
import Text.Parsec

data PGNMove = PawnMove (Maybe Hint) MoveType Square (Maybe OfficerType) |
               PieceMove PieceType (Maybe Hint) MoveType Square |
               Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving Show

castlesKingside = string "O-O" >> return (Castles Kingside)
castlesQueenside = string "O-O-O" >> return (Castles Queenside)

--pawnMove = do
--    s <- square
