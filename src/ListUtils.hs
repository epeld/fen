module ListUtils where
import Control.Monad.Plus

import Data.Maybe
import Data.Monoid
import Data.Foldable

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = case f a of
    Nothing -> a : []
    Just x -> a : iterateMaybe f x


compose :: Foldable t => t (a -> a) -> a -> a
compose = appEndo. foldMap Endo
