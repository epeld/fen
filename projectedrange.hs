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
    pieceType,
    )
import Square (Square)
import Position (
    Position, 
    readSquare,
    whoseTurn,
    enemyColor,
    enPassant,
    )
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

type SquareSeries = Range.SquareSeries

data ProjectedRange = Projected {
    range :: Range.Range,
    position :: Position
    } deriving (Show)

projectedRange :: MovingPiece.MovingPiece -> MoveType -> ProjectedRange
projectedRange mp mt =
    MovingPiece.range mp mt `project` MovingPiece.position mp

project r p = Projected r p

reaches mp = isJust . whichMoveType mp
whichMoveType mp d = find (mp `reachesBy` d) movetypes

reachesBy mp d = ProjectedRange.elem d. projectedRange mp

squares :: ProjectedRange -> [SquareSeries]
squares pr = projectSeries p r <$> Range.squares r
    where p = position pr
          r = range pr

elem :: Square -> ProjectedRange -> Bool
elem s pr = any (Prelude.elem s) (squares pr)

projectSeries p r ss =
    projectSeries' p (Range.moveType r) (Range.pieceType r) ss

projectSeries' :: Position -> MoveType -> PieceType -> SquareSeries
    -> SquareSeries
projectSeries' p Moves pt ss = takeWhile (isNothing . readSquare p) ss
projectSeries' p Takes pt ss = maybe [] (takeOnly' ss) enemyIx
    where enemyIx = firstEnemyIndex pt p ss

takeOnly i = drop (i-1) . take 1
takeOnly' = flip takeOnly

firstEnemyIndex :: PieceType -> Position -> SquareSeries -> Maybe Int
firstEnemyIndex Pawn p [s] = 
    if Just s == enPassant p || maybeIsEnemy p s
        then Just 0
        else Nothing
firstEnemyIndex Pawn p [] = Nothing
firstEnemyIndex Pawn p _ = error "firstEnemyIndex: something is wrong"
firstEnemyIndex _ p ss = findColoredPieceIndex (invert $ whoseTurn p) p ss

findColoredPieceIndex :: Color -> Position -> SquareSeries -> Maybe Int
findColoredPieceIndex c p = findIndex (maybeHasColor c p)

maybeHasColor c p = maybe False (hasColor c) . readSquare p
maybeIsEnemy p = maybeHasColor (enemyColor p) p
