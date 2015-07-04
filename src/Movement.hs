-- This module contains combinators for 'moving' between chess squares
-- TODO rename so as to not confuse with chess game move-logic
module Movement where
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

behind :: Color -> Square -> Maybe Square
behind color sq = safeHead $ concat $ apply1 sq [pawnMoveSource color]

type ApplyFn = (Square -> [Direction] -> [[Square]])

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

