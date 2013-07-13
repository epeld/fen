{-#LANGUAGE NoMonomorphismRestriction #-}
module Square(
    Square, square, square', file, rank,
    fileLetters, rankNumbers,
    Series, series,
    above, below, left, right,
    up, down, squares,
    upLeft, downLeft, upRight, downRight,
    twice, string, (!!!),
    Stepper, compose,
    internalTests
    ) where 
import Control.Applicative
import Control.Monad
import Data.List
import Data.Ix
import Data.Maybe
import Data.Char
import Test.HUnit

import TestUtils

type Stepper = Square -> Maybe Square
compose :: Stepper -> Stepper -> Stepper
compose = (>=>)

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

squares = [square' f r | f <- fileLetters, r <- rankNumbers ]
fileLetters = ['a'..'h']
rankNumbers = [1..8]

findFile f = find (==f) fileLetters
findRank r = find (==r) rankNumbers

square f r = liftM2 Square (findFile f) (findRank r)
string (Square f r) = f : show r

square' f r = fromJust $ square f r

type Series = [Square]

series (Square f r) (Square f2 r2)
    | f == f2 = [square' f r' | r' <- r `fromTo` r2]
    | r == r2 = [square' f' r | f' <- f `fromTo` f2]
    | abs df == abs dr =
        [square' f' r' | (f',r') <- zip (f `fromTo` f2) (r `fromTo` r2)]
    | otherwise = error "Square series invalid argument."
    where df = fromEnum f2 - fromEnum f
          dr = r2 - r

fromTo e e2 | i < i2 = e `enumFromTo` e2
            | otherwise = take di $ e `enumFromThen` (pred e)
    where i = fromEnum e
          i2 = fromEnum e2
          di = i - i2 + 1

testFromTo1 = do
    let s = 'a' `fromTo` 'c'
    assertLength 3 s
    assertEqual "first" 'a' $ head s

testFromTo2 = do
    let s = 'c' `fromTo` 'a'
    assertLength 3 s
    assertEqual "first" 'c' $ head s

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

internalTests = TestList $ [
    TestLabel "testFromTo1" $ TestCase testFromTo1,
    TestLabel "testFromTo2" $ TestCase testFromTo2
    ]
