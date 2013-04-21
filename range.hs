module Range (
    series,
    piece, moveType,
    ) where

import Data.Maybe (isNothing, fromJust, isJust)
import Control.Monad ((>=>), liftM)
import Control.Applicative ((<*>), (<$>))

import MoveType (
    MoveType (Takes, Moves)
    )
import Square (
    Square,
    SquareSeries,
    up, down, left, right,
    upLeft, upRight, downLeft, downRight,
    twice,
    rank,
    Stepper, compose,
    )
import Piece (
    OfficerType(..),
    Piece(..),
    PieceType(..),
    )
import Color ( 
    Color(..),
    )

data Type = Type {
    piece :: Piece,
    moveType :: MoveType
} deriving (Show, Eq)

series t s = map fromJust <$> validSquares
    where validSquares = takeUntilInvalid <$> seriesM t s
          validSquares :: [[Maybe Square]]

takeUntilInvalid = takeWhile isJust

seriesM :: Type -> Square -> [[Maybe Square]]
seriesM (Type (Piece Pawn c) Takes) = pawnTakesSeriesM c
seriesM (Type (Piece Pawn c) Moves) = pawnMovesSeriesM c
seriesM (Type (Piece (Officer t) _) _) = officerSeriesM t

pawnMovesSeriesM :: Color -> Square -> [[Maybe Square]]
pawnMovesSeriesM c = return. pawnMovesSingleSeriesM c
pawnMovesSingleSeriesM c s | initialRank = oneAndTwoSteps forward <*> [s]
                     | otherwise = oneStep forward <*> [s]
    where initialRank = rank s == initialPawnRank c
          forward = pawnDirection c

pawnTakesSeriesM :: Color -> Square -> [[Maybe Square]]
pawnTakesSeriesM c = return. pawnTakesSingleSeriesM c
pawnTakesSingleSeriesM c s = oneStep forwardAnd <*> [left,  right] <*> [s]
    where forwardAnd = (pawnDirection c >=>)

oneAndTwoSteps step = [step, twice step]
oneStep dir = [dir]

pawnDirection White = up
pawnDirection Black = down

initialPawnRank White = 2
initialPawnRank Black = 7

officerSeriesM t s | t `elem` longMovers = infiniteSeriesM
                   | otherwise = takeFirst <$> infiniteSeriesM
    where infiniteSeriesM = infiniteOfficerSeriesM t s
          takeFirst = take 1
          longMovers = [Queen, Rook, Bishop]

infiniteOfficerSeriesM t s = zipWith applyStepper steppers (repeat s)
    where steppers = officerSteppers t
          
applyStepper :: Square.Stepper -> Square -> [Maybe Square]
applyStepper st s = iterateStepper st <*> [s]

iterateStepper:: Square.Stepper -> [Square.Stepper]
iterateStepper s = iterate (compose s) s

officerSteppers :: OfficerType -> [Square.Stepper]
officerSteppers Knight = knightSteppers
officerSteppers Bishop = [upLeft, upRight, downLeft, downRight]
officerSteppers Rook = [up, down, left, right]
officerSteppers Queen = concat $ map officerSteppers [Rook, Bishop]
officerSteppers King = officerSteppers Queen
    
knightSteppers :: [Square.Stepper]
knightSteppers = 
    [onceTwice, twiceOnce] <*> [up, down] <*> [left, right]
    where onceTwice m m' = m' >=> twice m
          twiceOnce = flip onceTwice
