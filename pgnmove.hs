{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNMove where

import Square 
import Piece
import MoveType
import Piece
import PGNMoveEssentials
import CastlingSide

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
