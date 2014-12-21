module Piece where
import Prelude (Enum, toEnum, fromEnum, pred, succ, Bounded, maxBound, minBound, enumFrom)
import Data.Eq
import Data.Ord
import Data.Int
import Data.Function
import Text.Show


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

data Piece = Piece { pieceType :: PieceType, color :: Color } deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)

otherColor White = Black
otherColor Black = White

firstRank White = 2
firstRank Black = 7
