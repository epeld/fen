module Candidates where
import Data.Maybe
import Control.Lens
import Control.Applicative

import MoveType
import Piece
import Square
import Position
import PieceMovement


attackers :: Position -> Square -> [Square]
attackers p sq = candidates p Captures sq `concatMap` pieces
    where
    pieces = Piece <$> pieceTypes <*> pure (p ^. turn)
    pieceTypes = [Pawn .. Officer King]


-- Find all the candidate pieces that can reach a given square
-- using the specific move type.
--
-- Disregards whether the move would actually be valid. That is,
-- if e.g there is a piece to capture when using capture as move type
candidates :: Position -> MoveType -> Square -> Piece -> [Square]
candidates p mt sq pc = matchFirstPiece p pc `mapMaybe` movementFn mt pc sq 


-- Iterates over the sequence of squares until it finds one that is nonempty.
-- Then compares to the supplied piece and returns Just that square only if they match
matchFirstPiece :: Position -> Piece -> [Square] -> Maybe Square
matchFirstPiece p pc [] = Nothing
matchFirstPiece p pc (x : xs) = 
    case p ^. board . at x of
        Nothing -> 
            matchFirstPiece p pc xs

        Just pc' -> 
            if pc' == pc 
            then Just x
            else Nothing
