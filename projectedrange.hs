module ProjectedRange (ProjectedRange, project, ProjectedRange.elem,
                       inferMoveType, threatens, ) where

import Control.Monad (liftM)
import Data.Maybe (isNothing, isJust)
import Data.List (findIndex, find)

import MoveType (MoveType(..), movetypes)
import Piece (hasColor, PieceType(Pawn, Officer),)
import Color (Color, invert)
import MovingPiece ( MovingPiece, )
import Square (Square, Series)
import Range (Range, pieceType, moveType, range)
import qualified Range ( series, position)
import Position (Position, enemyColor, friendlyColor, enPassant, isEmpty,
                 isPawnCapturableSquare, containsFriendlyPiece,
                 containsEnemyPiece)

data ProjectedRange = ProjectedRange { series :: [Series] }

threatens :: Square -> MovingPiece -> Bool
threatens sq mp = ProjectedRange.elem sq pr
    where pr = project $ range mp Takes

elem s pr = any (Prelude.elem s) (series pr)

inferMoveType mp d = find isMoveType [Takes, Moves]
    where isMoveType = ProjectedRange.elem d. project. Range.range mp

project :: Range -> ProjectedRange
project r = ProjectedRange $ project' pt r
    where pt = pieceType r

project' :: PieceType -> Range -> [Series]

project' Pawn r = map (pawnProjection mt p) sqs
    where p = Range.position r
          sqs = Range.series r
          mt = moveType r

project' (Officer _) r =  map proj sqs
    where proj = projectSeries (Range.moveType r) p
          p = Range.position r
          sqs = Range.series r

pawnProjection Takes = projectPawnTakesSeries
pawnProjection Moves = projectPawnMovesSeries

projectPawnTakesSeries p sqs = filter (isPawnCapturableSquare p) sqs
projectPawnMovesSeries p sqs = takeWhile (isEmpty p) sqs

projectSeries :: MoveType -> Position -> Series -> Series
projectSeries Moves p s = takeWhile (isEmpty p) s
projectSeries Takes p s = maybe [] (flip take s. succ) (indexEnemy p s)
    where indexEnemy p s = findIndex (containsEnemyPiece p) s
