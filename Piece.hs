module Piece where
import Color(Color)

data Piece = Piece { pieceType :: PieceType, color :: Color }
             deriving (Show, Eq)

data PieceType = Pawn |
                 Officer OfficerType
                 deriving (Show, Eq)

data OfficerType = Knight | Bishop | Rook | Queen | King
                   deriving (Show, Eq, Ord, Enum)
