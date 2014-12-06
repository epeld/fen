module Square where

newtype Square = Square (Int, Int) deriving (Show, Eq)

type Offset = (Int, Int)
