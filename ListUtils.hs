module ListUtils where

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : maybe [] (iterateMaybe f) (f a)
