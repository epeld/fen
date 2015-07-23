module Movement where
import Control.Applicative ((<$>))

import Square
import ListUtils
import Piece
import Directions
import SquareOffsets

type RangeFn = [Direction] -> MovementFn
type MovementFn = Square -> [[Square]]

longRange :: RangeFn
longRange = sequence . fmap squares
    where
    squares d = drop 1 . iterateMaybe (`relative` d)


shortRange :: RangeFn
shortRange ds sq = take 1 <$> longRange ds sq 
