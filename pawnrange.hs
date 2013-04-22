module PawnRange ( pawnSeriesM) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad ((>=>))

import MovingPiece (color, square)
import Color ( Color(..),)
import Square ( Square, rank, file, left, right, up, down, twice)
import MoveType ( MoveType(..))

pawnSeriesM mp mt = (pawnSeriesM' mt) (color mp) (square mp)
    where pawnSeriesM' Moves = pawnMovesSeriesM
          pawnSeriesM' Takes = pawnTakesSeriesM

pawnMovesSeriesM :: Color -> Square -> [[Maybe Square]]
pawnMovesSeriesM c = return. pawnMovesSingleSeriesM c
pawnMovesSingleSeriesM c s 
    | initialRank = oneAndTwoSteps forward <*> [s]
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

