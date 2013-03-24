{-#LANGUAGE NoMonomorphismRestriction #-}
module Square(
    Square, square,
    file, rank,
    fileLetters, rankNumbers,
    SquareSeries,
    above, below, left, right,
    up, down,
    upLeft, downLeft, upRight, downRight,
    twice,
    (!!!)
    ) where 
import Control.Applicative
import Control.Monad
import Data.List
import Data.Ix
import Data.Maybe
import Data.Char

instance Enum (Square) where
    toEnum i = maybe (error "Bad argument") id (square f r)
        where f = toEnum $ fromEnum 'a' + (i `mod` 8)
              r = 1 + i `div` 8
            
    fromEnum (Square f r) = 8 * (fromEnum r - 1) + fromEnum f - fromEnum 'a'

m !!! s = m !! fromEnum s

data Square = Square {
    file :: Char,
    rank :: Int
} deriving (Show, Eq, Ix, Ord)

fileLetters = ['a'..'h']
rankNumbers = [1..8]

findFile f = find (==f) fileLetters
findRank r = find (==r) rankNumbers

square f r = liftM2 Square (findFile f) (findRank r)

type SquareSeries = [Square]

above s = square (file $ s) (succ . rank $ s)
below s = square (file $ s) (pred . rank $ s)
left s = square (pred $ file $ s) (rank s)
right s = square (succ$ file $ s) (rank s)
up = above
down = below

upLeft = above >=> left
upRight = above >=> right
downLeft = below >=> left
downRight = below >=> right

twice m = m >=> m
