module Square where
import Control.Monad (liftM2)

import Data.Char (intToDigit, digitToInt)

import FEN (FEN, encode, decode)
import Utils (findElem)

data Square = Square { file :: Char, rank :: Int }
              deriving (Eq, Ord)

coords (Square f r) = (f, r)

adjacentFiles :: Square -> Square -> Bool
adjacentFiles sq1 sq2 = adjacent (file sq1) (file sq2)

adjacent a b = let a' = fromEnum a
                   b' = fromEnum b
               in  abs (a' - b') == 1

ranks = [1..8]
files = ['a'..'h']

square :: Char -> Int -> Maybe Square
square f r = liftM2 Square (findElem f files) (findElem r ranks)

string :: Square -> String
string (Square f r) = f : intToDigit r : []


instance Show Square where
  show = string

instance FEN Square where
    encode (Square f r) = [f, intToDigit r]

    decode (f:r:[]) = let r' = digitToInt r
                       in square f r'
    decode _ = Nothing
