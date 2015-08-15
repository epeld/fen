{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module PgnParse where
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Text.Parsec
import Data.Char

import ParserUtils
import Parsable
import Move (Move(..))
import PartialMove (PartialMove)
import MoveType
import Piece
import MoveDescription (Description(..))
import PgnMove
import Castling


move :: Parser PgnMove
move = standardMove <|> castlingMove


standardMove :: Parser PgnMove
standardMove = fmap Right (choice [pawnMove, officerMove])


pawnMove :: Parser PartialMove
pawnMove = PawnMove <$> tryChoices [long, short] <*> optionMaybe promotion
    where
    
    -- E.g "exd4"
    long = do
        src <- tryChoices [squarePartial, filePartial]
        mt <- moveType
        dst <- square
        return $ Description (Just src) dst mt 


    --  E.g "e4"
    short = do
        dst <- square
        return $ Description Nothing dst Moves


    promotion = do
        char '='
        choose [Bishop .. Queen]


officerMove :: Parser PartialMove
officerMove = flip OfficerMove <$> officer <*> tryChoices [long, short]
    where

    -- E.g Ndxc3
    long = do
        src <- source 
        mt <- moveType
        dst <- square
        return $ Description (Just src) dst mt 


    -- E.g Nxc3
    short = do
        mt <- moveType
        dst <- square
        return $ Description Nothing dst mt


    source = tryChoices [squarePartial, rankPartial, filePartial]


    officer = choose [Bishop .. King]


castlingMove :: Parser PgnMove
castlingMove = fmap Left castlingSide
    

castlingSide :: Parser Side
castlingSide = choose [Queenside, Kingside]
