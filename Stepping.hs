module Stepping where
import Control.Monad ((>>=))
import Data.Maybe (isJust, catMaybe)

import Square (Square(Square), square)
import Color (Color(White, Black))
import Piece (Piece, PieceType, Officer(..))

type Stepper = Square -> Maybe Square

run :: Stepper -> Square -> [Square]
run step sq = catMaybe $ takeWhile isJust $ iterateM step sq

stepper :: OfficerType -> Square -> [Square]

steppers :: OfficerType -> [Stepper]
steppers King = steppers Queen
steppers Queen = steppers Rook ++ steppers Bishop
steppers Rook = [up, down, left, right]

steppers Bishop = map compose $
                  [[up, left], [up, right], [down, left], [down, right]]

steppers Knight = map compose $
                  [[up, up, left], [up, up, right],
                   [up, right, right], [down, right, right],
                   [down, down, right], [down, down, left],
                   [down, left, left], [up, left, left]]

compose :: [Stepper] -> Stepper
compose = foldl' (>=>) return

forward :: Color -> Stepper
forward White = up
forward Black = down

up :: Square -> Maybe Square
down :: Square -> Maybe Square
left :: Square -> Maybe Square
right :: Square -> Maybe Square

up = mv (1, 0)
down = mv (-1, 0)
right = mv (0, 1)
left = mv (0, -1)

mv :: (Int, Int) -> Square -> Maybe Square
mv x = uncurry square. inc2 x

--
-- Very general functions
--
inc :: Enum a => Int -> a -> a
inc x n = iterate (op n) x !! abs n

op :: Enum a => Int -> (a -> a)
op n = if n < 0
       then pred
       else succ

inc2 :: Enum a, Enum b => (Int, Int) -> (a, b) -> (a, b)
inc2 (x,y) (a,b) = (inc x a, inc y b)

iterateM f x = let x' = f x
               in x' : x' >>= iterate f
