module Square where
import Control.Arrow

data Square = Square { file :: Char, rank :: Int }
              deriving (Show, Eq, Ord)


adjacentFiles :: Square -> Square -> Bool
adjacentFiles sq1 sq2 = adjacent (file sq1) (file sq2)

adjacent a b = let a' = fromEnum a
                   b' = fromEnum b'
               in  abs (a' - b') == 1

ranks = [1..8]
files = ['a'..'h']


up :: Square -> Maybe Square
down :: Square -> Maybe Square
left :: Square -> Maybe Square
right :: Square -> Maybe Square

up = mv (1, 0)
down = mv (-1, 0)
right = mv (0, 1)
left = mv (0, -1)

upLeft :: Square -> Maybe Square
upRight :: Square -> Maybe Square
downLeft :: Square -> Maybe Square
downRight :: Square -> Maybe Square

upLeft = up >=> left
upRight = up >=> right
downLeft = down >=> left
downRight = down >=> right

square :: Char -> Int -> Maybe Square
square f r = liftM2 Square (findElem f files) (findElem r ranks)

mv :: (Int, Int) -> Square -> Maybe Square
mv x = uncurry square. inc2 x

inc2 (x,y) (a,b) = (inc x a, inc y b)
