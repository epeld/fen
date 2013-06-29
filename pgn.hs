{-#LANGUAGE NoMonomorphismRestriction #-}
module Pgn where

import Chess
import Square 
import Game
import Piece
import MonadOps
import Data.Char
import Data.List
import Text.Parsec
import Control.Monad
import Control.Applicative

data MoveType = Takes | Moves deriving (Show, Eq)

data PGNMoveEssentials = PGNMoveEssentials {
    hint :: (Maybe Hint),
    moveType :: MoveType,
    destination :: Square
    } deriving (Show, Eq)

data PGNMove = 
    PawnMove {
        essentials :: PGNMoveEssentials,
        promotion :: (Maybe OfficerType)
    } |
    OfficerMove {
        officerType :: OfficerType,
        essentials :: PGNMoveEssentials
    } |
    Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving (Show, Eq)

