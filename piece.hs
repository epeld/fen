module Piece where

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data Piece = Piece PieceType Color deriving (Show, Eq)

invert c = case c of
    White -> Black
    Black -> White
