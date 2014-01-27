module ListUtils where

reverseLookup :: [(a, b)] -> [(b, a)]
reverseLookup x = let (a,b) = unzip x
                   in zip b a
