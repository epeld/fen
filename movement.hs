module Movement (officerSquares, applyFn, ApplyFn, apply, apply1) where
import Prelude (Enum)
import Data.Maybe
import Data.Functor
import Data.Function
import Data.List
import Data.Int
import Data.Eq
import Text.Show

import Square (Square, Offset, add)
import ListUtils
import Piece
import Directions
import SquareOffsets

type ApplyFn = (Square -> [Direction] -> [[Square]])

officerSquares :: OfficerType -> Square -> [[Square]]
officerSquares officer sq = applyFn officer sq (directions officer)

apply :: ApplyFn
apply sq ds = fmap (seq sq) ds

apply1 :: ApplyFn
apply1 sq ds = applyN 1 sq ds

applyFn :: OfficerType -> ApplyFn
applyFn King = apply1
applyFn _ = apply

applyN :: Int -> ApplyFn
applyN n sq ds = fmap (take n) (apply sq ds)

seq :: Square -> Direction -> [Square]
seq sq d = iterateMaybe add' sq
    where add' = flip add (offset d)

