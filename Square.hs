module Square where
import Prelude ((+), (-), error)
import Data.Ord
import Data.Eq
import Data.Int
import Data.Maybe
import Data.List
import Data.Bool
import Data.Function
import Data.Char
import Data.String
import Data.Tuple
import Control.Applicative

import Text.Show

newtype Square = Square (Int, Int) deriving (Show, Eq, Ord)

type Offset = (Int, Int)

files :: [Char]
files = ['a'..'h']

ranks :: [Int]
ranks = [1..8]

square :: String -> Maybe Square
square [file, rank] = curry Square <$> fi <*> ri
    where fi = findIndex (== file) files
          ri = findIndex (== rank) ['1'..'8']
square _ = Nothing

square' :: String -> Square
square' s =
    case square s of
        Nothing -> error ("Invalid square string: '" ++ s ++ "'")
        Just sq -> sq

add :: Square -> Offset -> Maybe Square
add (Square (a, b)) (x, y) =
    let a' = a + x
        b' = b + y
     in if elem a' [1..8] && elem b' [1..8]
        then Just $ Square (a', b')
        else Nothing
                                 
rank (Square (_, b)) = b

diff :: Square -> Square -> (Int, Int)
diff (Square (a,b)) (Square (x,y)) = (a - x, b - y)
