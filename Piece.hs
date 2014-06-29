module Piece where
import Color(Color)

data Piece = Piece { pieceType :: PieceType, color :: Color }
             deriving (Show, Eq)

data PieceType = Pawn |
                 Officer OfficerType
                 deriving (Show, Eq)

data OfficerType = Knight | Bishop | Rook | Queen | King
                   deriving (Show, Eq, Ord, Enum)



instance FEN Piece where
    encode p = let pt = encode (pieceType p)
                in case color p of
                       White -> map toUpper pt
                       Black -> map toLower pt

    decode s@(x:[]) = let c = if isUpper x then White else Black
                       in Piece <$> decode s <*> pure c
    decode _ = Nothing


instance FEN PieceType where
    encode Pawn = "P"
    encode (Officer o) = encode o

    decode "P" = Just Pawn
    decode "p" = Just Pawn
    decode s = fmap Officer $ decode s


instance FEN OfficerType where
    encode Knight = "N"
    encode Bishop = "B"
    encode Rook = "R"
    encode King = "K"
    encode Queen = "Q"

    decode "N" = Just Knight
    decode "n" = Just Knight

    decode "B" = Just Bishop
    decode "b" = Just Bishop

    decode "R" = Just Rook
    decode "r" = Just Rook

    decode "K" = Just King
    decode "k" = Just King

    decode "Q" = Just Queen
    decode "q" = Just Queen

    decode _ = Nothing
