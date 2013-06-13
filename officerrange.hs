module OfficerRange (officerSeriesM) where

import Control.Monad ((>=>))
import Control.Applicative ((<*>), (<$>))

import Square ( Square, Series, up, down, left, right,
                upLeft, upRight, downLeft, downRight,
                twice, rank, Stepper, compose,)
import Color ( Color(..),)
import Piece ( OfficerType(..), PieceType(..),)
import MovingPiece ( MovingPiece, officerType, square,
                     piece, pieceType,)

officerSeriesM mp _ = officerSeriesM' (officerType mp) (square mp)
officerSeriesM' t s | t `elem` longMovers = infiniteSeriesM
                    | otherwise = take 1 <$> infiniteSeriesM
    where infiniteSeriesM = infiniteOfficerSeriesM t s
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
