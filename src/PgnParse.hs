{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
module PgnParse where
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Text.Parsec
import Data.Char

import ParserUtils
import Parsable
import MoveType
import Piece
import PartialMove
import PgnMove


move = standardMove <|> castlingMove


standardMove :: Parser PgnMove
standardMove = fmap Right (pawnMove <|> officerMove)


pawnMove :: Parser PartialMove
pawnMove = Move.PawnMove <$> choice [long, short] <*> optionMaybe promotion
    where
    
    -- E.g "exd4"
    long = do
        src <- filePartial <|> squarePartial
        mt <- moveType
        dst <- square
        return $ Partial.Description dst mt (Just src)


    --  E.g "e4"
    short = do
        dst <- square
        return $ Partial.Description dst Moves Nothing


    promotion = do
        char '='
        choose [Bishop .. Queen]


officerMove :: Parser PartialMove
officerMove = Move.OfficerMove <$> officer <*> choice [long, short]
    where

    -- E.g Ndxc3
    long = do
        src <- source 
        mt <- moveType
        dst <- square
        return $ Partial.Description dst mt (Just src)


    -- E.g Nxc3
    short = do
        mt <- moveType
        dst <- square
        return $ Partial.Description dst mt Nothing


    source = choice [rankPartial, filePartial, squarePartial]


castlingMove :: Parser PgnMove
castlingMove = fmap Left castlingSide
    

castlingSide :: Parser Side
castlingSide = choose [Queenside, Kingside]
