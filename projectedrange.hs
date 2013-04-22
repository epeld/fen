module ProjectedRange (
    ProjectedRange,
    SquareSeries,
    ProjectedRange.elem,
    projectedRange,
    squares,
    project,
    range,
    position,
    whichMoveType,
    reaches, reachesBy,
    ) where
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Maybe (isNothing, isJust)
import Data.List (findIndex, find)

import MovingPiece ( MovingPiece, position,)
import qualified Range ( series,)
import Square (Square, SquareSeries)
import Range ( Range,)
import Position ( Position, enemyColor, friendlyColor, enPassant,)
import MoveType (
    MoveType(..),
    movetypes
    )
import Piece (
    hasColor,
    PieceType(Pawn),
    )
import Color (
    Color,
    invert
    )
data ProjectedRange = ProjectedRange { series :: [SquareSeries] }

project :: Range -> ProjectedRange
project r = ProjectedRange $ project' r
project' r = map (projectSeries $ position r) (Range.series r)

projectSeries :: Position -> SquareSeries -> SquareSeries
projectSeries p s = take ixFirstStop
    where ixFirstStop = maybe (length s) (findFirstStop p s)

findFirstStop p s = min' enemyIx friendlyIx
    where enemyIx = (+1) <$> findPieceColor (enemyColor p)
          friendlyIx = findPieceColor (friendlyColor p)

findPieceColor _ = Just 3

min' Nothing a = a
min' a Nothing = a
min' a b = min a b
