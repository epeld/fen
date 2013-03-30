module ProjectedRange where
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Maybe (isNothing)
import Data.List (find)
import Prelude hiding (
    min
    )

import qualified MovingPiece (
    MovingPiece,
    range,
    position,
    pieceType,
    )
import qualified Range (
    Range,
    SquareSeries,
    moveType,
    squares,
    )
import Square (Square)
import Position (
    Position, 
    readSquare,
    whoseTurn,
    )
import MoveType (
    MoveType(..)
    )
import Piece (hasColor)
import Color (invert)

data ProjectedRange = Projected {
    range :: Range.Range,
    position :: Position
    } deriving (Show)

projectedRange :: MovingPiece.MovingPiece -> MoveType -> ProjectedRange
projectedRange mp mt =
    MovingPiece.range mp mt `project` MovingPiece.position mp

project r p = Projected r p

type SquareSeries = Range.SquareSeries
squares :: ProjectedRange -> [SquareSeries]
squares pr = projectSeries p (Range.moveType r) <$> Range.squares r
    where accept = True
          p = position pr
          r = range pr

elem :: Square -> ProjectedRange -> Bool
elem s pr = any (Prelude.elem s) (squares pr)

projectSeries :: Position -> MoveType -> SquareSeries -> SquareSeries
projectSeries p Moves ss = takeWhile (isNothing . readSquare p) ss
projectSeries p Takes ss = take stop ss
    where stop = maybe (length ss) id (firstStop p ss)

min mi mi2 = max (liftM negate mi) (liftM negate mi2)

firstStop :: Position -> SquareSeries -> Maybe Int
firstStop p ss = firstStop' (whoseTurn p) ss
firstStop' c ss = min (firstFriendly c ss) (succ <$> firstEnemy c ss)

findColoredPiece c = find $ maybe False (hasColor c)
firstFriendly = findColoredPiece
firstEnemy c ss = findColoredPiece . invert
