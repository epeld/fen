module Square where 
import Control.Monad
import Control.Applicative
import Data.List
import Piece

data File = File Char
data Rank = Rank Int
data Square = Square File Rank
files = "abcdefgh"

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

up1 c = case c of
    White -> -1
    Black ->  1

down1 = up1 . invert

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
    ix <- liftM (+i) $ findIndex (==x) xs
    case ix < 0 || ix >= length xs of
        False -> fail $ "Can't move that far"
        True -> return $ xs !! ix

moveFile = move files

hoffset :: Square -> Int -> Maybe Square
hoffset sq i = do
    let f = case fileOf sq of File f -> f
    f2 <-  moveFile f i
    return $ Square (File f2) (rankOf sq)

