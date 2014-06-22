module Stepping where
import Control.Monad ((>>=), (>=>))

import Data.List (scanl, foldl')
import Data.Maybe (isJust, catMaybes)

import Square (Square(Square), square, coords)
import Color (Color(White, Black))
import Piece (Piece, PieceType(..), OfficerType(..))

import Utils

type Stepper = Square -> Maybe Square

run :: Stepper -> Square -> [Square]
run step sq = catMaybes $ takeWhile isJust $ iterateM step sq

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
mv x = uncurry square. inc2 x. coords
