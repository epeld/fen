module Piece where

import Data.Char

-- Differentiate pawns from officers
data PieceType = Pawn | Officer OfficerType deriving (Show, Eq)
data OfficerType = Bishop | Knight | Rook | Queen | King deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data Piece = Piece {
    pieceType :: PieceType,
    color :: Color
    } deriving (Show, Eq)

invert c = case c of
    White -> Black
    Black -> White

charToOfficerType c = case toLower c of
    'r' -> Rook
    'b' -> Bishop
    'k' -> King
    'q' -> Queen
    'n' -> Knight
