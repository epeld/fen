{-# LANGUAGE TemplateHaskell #-}
module Piece where
import Control.Lens

instance Enum PieceType where
    toEnum 0 = Pawn
    toEnum ix = Officer $ toEnum (pred ix)

    fromEnum Pawn = 0
    fromEnum (Officer ot) = succ $ fromEnum ot

    enumFrom pt = [pt .. Officer King]

instance Bounded PieceType where
    minBound = Pawn
    maxBound = Officer King

data PieceType = Pawn | Officer OfficerType 
                 deriving (Show, Eq, Ord)

data OfficerType = Bishop | Knight | Rook | Queen | King
                   deriving (Show, Eq, Ord, Enum)

data Piece = Piece { _pieceType :: PieceType, _color :: Color } deriving (Show, Eq, Ord)

data Color = White | Black deriving (Show, Eq, Enum, Ord)

otherColor White = Black
otherColor Black = White

firstRank White = 2
firstRank Black = 7

makeLenses ''Piece
