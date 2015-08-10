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
standardMove = do
    mv <- choice [pawnMove, officerMove]
    return (Right mv)


pawnMove :: Parser PartialMove
pawnMove = Move.PawnMove <$> desc <*> optionMaybe promotion
    where
    desc = choice [long, short]
    
    -- E.g "exd4"
    long = do
        src <- choice [filePartial, squarePartial]
        mt <- moveType
        dst <- square
        return $ Partial.Description dst mt (Just src)


    --  E.g "e4"
    short = do
        dst <- square
        return $ Partial.Description dst Moves Nothing


    promotion = do
        char '='
        Parsable.anyOf [Bishop .. Queen]


officerMove :: Parser PartialMove
officerMove = Move.OfficerMove <$> officer <*> desc
    where
    desc = choice [long, short]


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
castlingMove = Left `fmap` castlingSide
    

castlingSide :: Parser Side
castlingSide = Parsable.anyOf [Queenside, Kingside]
