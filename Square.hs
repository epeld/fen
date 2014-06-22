module Square where
import Control.Monad (liftM2)

import Utils (findElem)

data Square = Square { file :: Char, rank :: Int }
              deriving (Show, Eq, Ord)

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
