{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNMove where

import Square 
import Piece
import MoveType
import ChessMove
import Piece
import PGNMoveEssentials
import CastlingSide
import ErrorMonad
import Position

data PGNMove = 
    PawnMove {
        essentials :: Essentials,
        promotion :: Maybe OfficerType
    } |
    OfficerMove {
        officerType :: OfficerType,
        essentials :: Essentials
    } |
    Castles Side deriving (Show, Eq)

translate :: PGNMove -> Position -> ErrorMonad ChessMove
translate mv p = error "hej"
