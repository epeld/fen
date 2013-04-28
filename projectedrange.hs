module ProjectedRange ( ProjectedRange, project,) where

import Control.Monad (liftM)
import Data.Maybe (isNothing, isJust)
import Data.List (findIndex, find)

import MovingPiece ( MovingPiece, )
import Square (Square, SquareSeries)
import Range ( Range,)
import Position ( Position, enemyColor, friendlyColor, enPassant,
                  isCapturableSquare, containsFriendlyPiece)
import MoveType ( MoveType(..), movetypes)
import Piece ( hasColor, PieceType(Pawn),)
import Color ( Color, invert)
import qualified Range ( series, position)

data ProjectedRange = ProjectedRange { series :: [SquareSeries] }

project :: Range -> ProjectedRange
project r = ProjectedRange $ project' r

project' :: Range -> [SquareSeries]
project' r = map (projectSeries $ Range.position r) (Range.series r)

projectSeries :: Position -> SquareSeries -> SquareSeries
projectSeries p s = take ixFirstStop s
    where ixFirstStop = maybe (length s) id (indexFirstStop p s)

indexFirstStop p s = min' enemyIx friendlyIx
    where enemyIx = liftM (+1) (indexCapturable p s)
          friendlyIx = indexFriendly p s
          
indexCapturable p s = findIndex (isCapturableSquare p) s
indexFriendly p s = findIndex (containsFriendlyPiece p) s

min' Nothing a = a
min' a Nothing = a
min' a b = min a b
