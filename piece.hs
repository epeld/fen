module Piece where
import PieceType
import Color


data Piece = Piece {
    pieceType :: PieceType,
    color :: Color} deriving (Show, Eq)


piece :: Char -> Piece
piece c = Piece {
  pieceType = pieceType c,
  color = color c}
