{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNParse where

import Square hiding (file, rank)
import Game
import Board
import Piece
import qualified PGNMove
import Data.Char
import Text.Parsec
import Control.Applicative hiding ((<|>))

castlesKingside = string "O-O" >> return (Pgn.Castles Kingside)
castlesQueenside = string "O-O-O" >> return (Pgn.Castles Queenside)

castles = try castlesQueenside <|> castlesKingside

moveType = option Pgn.Moves (char 'x' >> return Pgn.Takes) 

-- "exd4"
longPawnMove = do
    h <- pawnHint
    m <- moveType
    s <- square
    p <- optionMaybe promotion
    let e = Pgn.PGNMoveEssentials (Just h) m s
    return $ Pgn.PawnMove e p

-- "e4=D"
shortPawnMove = do
    s <- square
    p <- optionMaybe promotion
    let e = Pgn.PGNMoveEssentials Nothing Pgn.Moves s
    return $ Pgn.PawnMove e p

promotion = string "=" >> charToOfficerType <$> oneOf "RBQN"

squareHint = Pgn.SquareHint <$> square 
fileHint = Pgn.FileHint <$> file 
rankHint = Pgn.RankHint <$> rank 
file = File <$> oneOf ['a'..'h'] 
rank = Rank . digitToInt <$> oneOf ['1'..'8']

officerHint = try rankHint <|> pawnHint
pawnHint = try squareHint <|> fileHint

pawnMove = choice [try longPawnMove, shortPawnMove]

officerType = charToOfficerType <$> oneOf "RKNQB"

officerMove = choice [try longOfficerMove, shortOfficerMove]

shortOfficerMove = do
    t <- officerType
    m <- moveType
    s <- square
    let e = Pgn.PGNMoveEssentials Nothing m s
    return $ Pgn.OfficerMove t e

longOfficerMove = do
    t <- officerType
    h <- officerHint
    m <- moveType
    s <- square
    let e = Pgn.PGNMoveEssentials (Just h) m s
    return $ Pgn.OfficerMove t e

pgnMove = choice [try pawnMove, try officerMove, castles]

