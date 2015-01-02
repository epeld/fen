module ListUtils where
import Control.Monad.Plus

import Data.Monoid
import Data.Foldable

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : maybe [] (iterateMaybe f) (f a)

safeHead :: [a] -> Maybe a
safeHead = mfromList

compose :: Foldable t => t (a -> a) -> a -> a
compose = appEndo. foldMap Endo