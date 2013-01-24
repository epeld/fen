{-#LANGUAGE NoMonomorphismRestriction #-}
module Pgn where

import Square 
import Game
import Piece
import Data.Char
import Data.List
import Text.Parsec
import Control.Monad

data MoveType = Takes | Moves deriving (Show, Eq)

data PGNMoveEssentials = PGNMoveEssentials {
    hint :: (Maybe Hint),
    moveType :: MoveType,
    destination :: Square
    } deriving (Show, Eq)

data PGNMove = PawnMove {
    essentials :: PGNMoveEssentials,
    promotion :: (Maybe OfficerType)
    } |
    OfficerMove {
        officerType :: OfficerType,
        essentials :: PGNMoveEssentials
    } |
    Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving (Show, Eq)

hintFromMove m = hint . essentials $ m

fand = liftM2 (&&)

candidates :: Game -> PGNMove -> [Int]
candidates g mv =
    let 
        h  = maybe (return False) matchHintX (hintFromMove mv)
        pt p = Pgn.pieceType mv == Piece.pieceType p
        c p = whoseMove (properties g) == color p
        match = maybe False (c `fand` pt)
     in 
        filter h $ findIndices match (board g)

matchHint :: Hint -> Square -> Bool
matchHint h s =
    case h of
        FileHint f -> f == file s
        RankHint r -> r == rank s
        SquareHint s2 -> s2 == s

matchHintX h i = matchHint h (allSquares !! i)

pieceType mv =
    case mv of
        PawnMove _ _ -> Pawn
        OfficerMove t _ -> Officer t
