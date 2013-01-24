{-#LANGUAGE NoMonomorphismRestriction #-}
module Square where 
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ix
import Data.Maybe
import Data.Char
import Piece
import Text.Parsec

instance Enum (File) where
    toEnum i = let i' = fromEnum 'a' + i 
                in File (toEnum i')
    fromEnum (File c) = fromEnum c - fromEnum 'a'
instance Enum (Rank) where
    toEnum i = let i' = i + 1
                in Rank (toEnum i')
    fromEnum (Rank c) = fromEnum c - 1
instance Enum (Square) where
    toEnum i = Square (toEnum $ mod i 8 ) (toEnum $ div i 8)
    fromEnum (Square f r) = 8 * (fromEnum r) + fromEnum f

data File = File Char deriving (Show, Eq, Ix, Ord)
data Rank = Rank Int deriving (Show, Eq, Ix, Ord)
data Square = Square {
    file :: File,
    rank :: Rank
    } deriving (Show, Eq, Ix, Ord)
files = "abcdefgh"
ranks = [1..8]

allSquares = []

parseSquare s = parse square s s

square' s = case parseSquare s of
    Right s -> s
    Left _ -> error "can't read square"

fileOf (Square f _) = f
rankOf (Square _ r) = r

up1 :: Color -> Int
up1 c = case c of
    White -> -1
    Black ->  1

down1 = up1 . invert

left1 :: Color -> Int
left1 c = case c of
    White -> -1
    Black -> 1

right1 = left1 . invert

relRankNr i c = case c of
    White -> i
    Black -> 8 - i

isRankNr i c (Rank r) =
    let i' = relRankNr i c
     in r == i'

--offset s o = flip voffset (snd o) $ hoffset s (fst o)

move :: Eq a => [a] -> a -> Int -> Maybe a
move xs x i = do 
    ix <- (+i) <$> findIndex (==x) xs
    case ix < 0 || ix >= length xs of
        True -> fail $ "Can't move that far"
        False -> return $ xs !! ix


moveRank = move ranks
moveFile = move files

hoffset :: Square -> Int -> Maybe Square
hoffset sq i = do
    let f = case fileOf sq of File f -> f
    f2 <-  moveFile f i
    return $ Square (File f2) (rankOf sq)

voffset sq i = do
    let r = case rankOf sq of Rank r -> r
    r2 <- moveRank r i
    return $ Square (fileOf sq) (Rank r2)

offset :: Square -> (Int, Int) -> Maybe Square
offset s (h,v) = hoffset s h >>= flip voffset v
offsets s m = let maybes = map (offset s) m
               in fromJust $ sequence $ takeWhile isJust maybes

hoffsets s = offsets s . flip zip [0,0..]
voffsets s = offsets s . zip [0,0..]

manyLeftOf s  = hoffsets s [1..]
manyRightOf s = hoffsets s [-1,-2..]
manyAbove s = voffsets s [1..]
manyBelow s = voffsets s [-1,-2..]

tupMul (a,b) i = (i*a, i*b)

squareSeries s t = offsets s $ map (tupMul t) [1..]
squareSeriesH s h = squareSeries s (h,0)
squareSeriesV s v = squareSeries s (0,v)

square = do
    f <- oneOf ['a'..'h'] 
    r <- digitToInt <$> oneOf ['1'..'8']
    return $ Square (File f) (Rank r)
