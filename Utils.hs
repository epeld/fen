module Utils where
import Prelude ((==), Eq, Show, Int, Enum, pred, succ, (<), abs, (!!), ($), flip)
import Control.Monad (return, Monad, (>>=), (>=>))

import Data.List (scanl, foldl', find, map, repeat, iterate)
import Data.Maybe (isJust, catMaybes, Maybe, maybe)
import Data.Bool (Bool(False))

forEach :: [a] -> (a -> b) -> [b]
forEach = flip map

findElem :: Eq a => a -> [a] -> Maybe a
findElem a = find (== a)

maybeNot :: Monad m => (a -> m Bool) -> Maybe a -> m Bool
maybeNot = maybe (return False)

inc :: Enum a => Int -> a -> a
inc n x = iterate (op n) x !! abs n

op :: Enum a => Int -> (a -> a)
op n = if n < 0
       then pred
       else succ

inc2 :: (Enum a, Enum b) => (Int, Int) -> (a, b) -> (a, b)
inc2 (x,y) (a,b) = (inc x a, inc y b)

iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f x = let iterations = scanl (>=>) f (repeat f)
               in map ($ x) iterations
