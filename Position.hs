module Position where
import Prelude ()
import Data.Bool
import Control.Monad.Reader

data Position = Position

type PReader = Reader Position

legal :: Position -> Bool
legal _ = True
