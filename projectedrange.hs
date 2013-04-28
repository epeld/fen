module ProjectedRange ( ProjectedRange, project,) where

import Control.Monad (liftM)
import Data.Maybe (isNothing, isJust)
import Data.List (findIndex, find)

import MovingPiece ( MovingPiece, )
import Square (Square, SquareSeries)
import Range ( Range, pieceType)
import Position ( Position, enemyColor, friendlyColor, enPassant,
                  isCapturableSquare, containsFriendlyPiece)
import MoveType ( MoveType(..), movetypes)
import Piece ( hasColor, PieceType(Pawn),)
import Color ( Color, invert)
import qualified Range ( series, position)

data ProjectedRange = ProjectedRange { series :: [SquareSeries] }

project :: Range -> ProjectedRange
project r = ProjectedRange $ project' pt r
    where pt = pieceType r

project' :: PieceType -> Range -> [SquareSeries]
project' Pawn r = []
project' _ r = map (projectSeries $ Range.position r) (Range.series r)

projectSeries :: Position -> SquareSeries -> SquareSeries
projectSeries p s = take ixFirstStop s
    where ixFirstStop = indexFirstStop p s

indexFirstStop p s = min enemyIx friendlyIx
    where enemyIx = maybeEndOfSeries (+1) (indexCapturable p s)
          friendlyIx = maybeEndOfSeries id (indexFriendly p s)
          maybeEndOfSeries = maybe (length s)
          
indexCapturable p s = findIndex (isCapturableSquare p) s
indexFriendly p s = findIndex (containsFriendlyPiece p) s
