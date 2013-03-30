module ProjectedRange (
    ProjectedRange,
    SquareSeries,
    ProjectedRange.elem,
    projectedRange,
    squares,
    project,
    range,
    position,
    ) where
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

type SquareSeries = Range.SquareSeries

data ProjectedRange = Projected {
    range :: Range.Range,
    position :: Position
    } deriving (Show)

projectedRange :: MovingPiece.MovingPiece -> MoveType -> ProjectedRange
projectedRange mp mt =
    MovingPiece.range mp mt `project` MovingPiece.position mp

project r p = Projected r p

squares :: ProjectedRange -> [SquareSeries]
squares pr = projectSeries p (Range.moveType r) <$> Range.squares r
    where p = position pr
          r = range pr

elem :: Square -> ProjectedRange -> Bool
elem s pr = any (Prelude.elem s) (squares pr)

projectSeries :: Position -> MoveType -> SquareSeries -> SquareSeries
projectSeries p Moves ss = takeWhile (isNothing . readSquare p) ss
projectSeries p Takes ss = maybe [] (takeOnly' ss) enemyIx
    where enemyIx = firstEnemyIndex p ss

takeOnly i = drop (i-1) . take 1
takeOnly' = flip takeOnly

firstEnemyIndex :: Position -> SquareSeries -> Maybe Int
firstEnemyIndex p = findColoredPieceIndex (invert $ whoseTurn p) p

findColoredPieceIndex :: Color -> Position -> SquareSeries -> Maybe Int
findColoredPieceIndex c p = findIndex $ maybe False (hasColor c) . readSquare p
