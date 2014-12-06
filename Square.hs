module Square where
import Prelude ()
import Data.Ord
import Data.Eq
import Data.Int
import Text.Show

newtype Square = Square (Int, Int) deriving (Show, Eq, Ord)

type Offset = (Int, Int)
