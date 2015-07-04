{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module PgnParse where
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Text.Parsec
import Data.Char

import MoveType
import Piece
import PartialMove
import qualified Square
import qualified Move

import qualified PartialDescription as Partial

type Parser r = forall s. forall m. Stream s m Char => ParsecT s () m r

--pgnMove = choice [try pawnMove, try officerMove, castles]

pawnMove :: Parser PartialMove
pawnMove = Move.PawnMove <$> desc <*> optionMaybe promotion
    where
    desc = choice [try longPawnMoveDesc, shortPawnMoveDesc]

officerMove :: Parser PartialMove
officerMove = Move.OfficerMove <$> officer <*> desc
    where
    desc = choice [try longOfficerMoveDesc, shortOfficerMoveDesc]

--castlesKingside = string "O-O" >> return (Pgn.Castles Kingside)
--castlesQueenside = string "O-O-O" >> return (Pgn.Castles Queenside)

--castles = try castlesQueenside <|> castlesKingside


shortOfficerMoveDesc :: Parser Partial.Description
shortOfficerMoveDesc = do
    mt <- moveType
    dst <- square
    return $ Partial.Description dst mt Nothing


longOfficerMoveDesc :: Parser Partial.Description
longOfficerMoveDesc = do
    src <- officerSource
    mt <- moveType
    dst <- square
    return $ Partial.Description dst mt (Just src)


-- E.g "exd4"
longPawnMoveDesc :: Parser Partial.Description
longPawnMoveDesc = do
    src <- filePartial <|> squarePartial
    mt <- moveType
    dst <- square
    return $ Partial.Description dst mt (Just src)


--  E.g "e4"
shortPawnMoveDesc :: Parser Partial.Description
shortPawnMoveDesc = do
    dst <- square
    return $ Partial.Description dst Moves Nothing
    

moveType :: Parser MoveType
moveType = Moves `option` captures
    where
    captures = char 'x' >> return Captures


promotion :: Parser OfficerType
promotion = do
    char '='
    rook <|> knight <|> bishop <|> queen


rook :: Parser OfficerType
rook = char 'R' >> return Rook


knight :: Parser OfficerType
knight = char 'N' >> return Knight


bishop :: Parser OfficerType
bishop = char 'B' >> return Bishop


queen :: Parser OfficerType
queen = char 'Q' >> return Queen


king :: Parser OfficerType
king = char 'K' >> return King


officer :: Parser OfficerType
officer = king <|> queen <|> rook <|> bishop <|> knight


officerSource :: Parser Partial.PartialSquare
officerSource = choice [rankPartial, filePartial, squarePartial]


rankPartial :: Parser Partial.PartialSquare
rankPartial = Partial.File <$> file


filePartial :: Parser Partial.PartialSquare
filePartial = Partial.Rank <$> rank


squarePartial :: Parser Partial.PartialSquare
squarePartial = Partial.Whole <$> square


square :: Parser Square.Square
square = fromJust <$> liftM2 Square.square' file rank


file :: Parser Char
file = oneOf "abcdefgh"


rank :: Parser Int
rank = digitToInt <$> oneOf "12345678"
