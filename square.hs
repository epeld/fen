module Square where 
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ix
import Data.Maybe
import Piece

data File = File Char deriving (Show, Eq, Ix, Ord)
data Rank = Rank Int deriving (Show, Eq, Ix, Ord)
data Square = Square File Rank deriving (Show, Eq, Ix, Ord)
files = "abcdefgh"
ranks = [1..8]

rank r = let i = read [r]
          in if 1 <= i && i <= 8 
              then return $ Rank i
              else fail $ "Not a rank: " ++ [r]

file f = if f `elem` "abcdefgh" 
    then return $ File f
    else fail $ "Not a file: " ++ [f]

square [f,r] = liftM2 Square (file f) (rank r)

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

--offset s o = flip voffset (snd o) $ hoffset s (fst o)

absDec x = case x < 0 of
    True -> x + 1
    False -> x - 1


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
