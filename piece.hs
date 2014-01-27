module Piece where
import PieceType
import Color


data Piece = Piece {
    pieceType :: PieceType,
    color :: Color} deriving (Show, Eq)


piece :: Char -> Maybe Piece
piece c = do
  let clr = color c
  pt <- pieceType c
  Just $ Piece {pieceType = pt, color = clr}
