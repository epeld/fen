module ProjectedRange where
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Maybe (isNothing)
import Data.List (findIndex)

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
import Color (
    Color,
    invert
    )

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

min' mi mi2 = max (liftM negate mi) (liftM negate mi2)

firstStop :: Position -> SquareSeries -> Maybe Int
firstStop p ss = 
    min' (firstFriendlyIndex c p ss) (succ <$> firstEnemyIndex c p ss)
    where c = whoseTurn p

findColoredPieceIndex :: Color -> Position -> SquareSeries -> Maybe Int
findColoredPieceIndex c p = findIndex $ maybe False (hasColor c) . readSquare p
firstFriendlyIndex = findColoredPieceIndex
firstEnemyIndex = findColoredPieceIndex . invert
