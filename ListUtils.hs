module ListUtils where
import Data.Monoid
import Data.Foldable

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : maybe [] (iterateMaybe f) (f a)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

compose :: Foldable t => t (a -> a) -> a -> a
compose = appEndo. foldMap Endo
