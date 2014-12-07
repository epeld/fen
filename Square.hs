module Square where
import Prelude ((+))
import Data.Ord
import Data.Eq
import Data.Int
import Data.Maybe
import Data.List
import Data.Bool
import Data.Function

import Text.Show

newtype Square = Square (Int, Int) deriving (Show, Eq, Ord)

type Offset = (Int, Int)

add :: Square -> Offset -> Maybe Square
add (Square (a, b)) (x, y) =
    let a' = a + x
        b' = b + y
     in if elem a' [1..8] && elem b' [1..8]
        then Just $ Square (a', b')
        else Nothing
                                 
