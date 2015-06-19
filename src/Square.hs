module Square where
import Prelude ((+), (-), error, succ)
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

string :: Square -> String
string (Square (a, b)) = [chr (ord 'a' + a - 1), chr (ord '1' + b - 1)]

unsafe :: String -> Square
unsafe s = sq (square s)
    where sq Nothing = error ("Invalid square " ++ s)
          sq (Just x) = x

square :: String -> Maybe Square
square [file, rank] = curry Square <$> fi <*> ri
    where fi = succ <$> findIndex (== file) files
          ri = succ <$> findIndex (== rank) ['1'..'8']
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
    -- TODO use <,> instead of elem
     in if elem a' [1..8] && elem b' [1..8]
        then Just $ Square (a', b')
        else Nothing
                                 
rank :: Square -> Int
rank (Square (_, b)) = b

file :: Square -> Int
file (Square (a, _)) = a

fileIndex :: Char -> Maybe Int
fileIndex f = findIndex (== f) files

diff :: Square -> Square -> (Int, Int)
diff (Square (a,b)) (Square (x,y)) = (a - x, b - y)
