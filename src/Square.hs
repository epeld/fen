module Square where
import Control.Monad
import Control.Applicative
import Data.List
import Data.Char

newtype Square = Square (Int, Int) deriving (Show, Eq, Ord)

type Offset = (Int, Int)

files :: [Char]
files = ['a'..'h']

ranks :: [Int]
ranks = [1..8]

string :: Square -> String
string (Square (a, b)) = [chr (ord 'a' + a - 1), chr (ord '1' + b - 1)]

square :: String -> Maybe Square
square [file, rank] = digitToMaybeInt rank >>= square' file 
square _ = Nothing

digitToMaybeInt :: Char -> Maybe Int
digitToMaybeInt c = if isDigit c then Just $ digitToInt c else Nothing

square' :: Char -> Int -> Maybe Square
square' f r = curry Square <$> fi <*> ri
    where fi = succ <$> findIndex (== f) files
          ri = succ <$> findIndex (== r) ranks


unsafe :: String -> Square
unsafe s =
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
