{-#LANGUAGE NoMonomorphismRestriction #-}
module MonadOps where

import Control.Monad

mAnd = liftM2 (&&)
