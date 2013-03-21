{-#LANGUAGE NoMonomorphismRestriction #-}
module Square(
    Square,
    square,
    file,
    rank,
    fileLetters,
    rankNumbers,
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
