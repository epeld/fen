{-#LANGUAGE NoMonomorphismRestriction #-}
module PgnParse where

import Square hiding (file, rank)
import Game
import Board
import Piece
import Pgn
import Data.Char
import Text.Parsec

castlesKingside = string "O-O" >> return (Castles Kingside)
castlesQueenside = string "O-O-O" >> return (Castles Queenside)

castles = try castlesQueenside <|> castlesKingside

moveType = option Moves (char 'x' >> return Takes) 

longPawnMove = do
    h <- pawnHint
    m <- moveType
    s <- square
    p <- optionMaybe promotion
    let e = PGNMoveEssentials (Just h) m s
    return $ PawnMove e p

shortPawnMove = do
    s <- square
    p <- optionMaybe promotion
    let e = PGNMoveEssentials Nothing Moves s
    return $ PawnMove e p

promotion = string "=" >> oneOf "RBQN" >>= return . charToOfficerType

squareHint = square >>= return . SquareHint 
fileHint = file >>= return . FileHint
rankHint = rank >>= return . RankHint
file = oneOf ['a'..'h'] >>= return . File
rank = oneOf ['1'..'8'] >>= return . Rank . digitToInt

officerHint = try rankHint <|> pawnHint
pawnHint = try squareHint <|> fileHint

pawnMove = choice [try longPawnMove, shortPawnMove]

officerType = oneOf "RKNQB" >>= return . charToOfficerType

officerMove = choice [try longOfficerMove, shortOfficerMove]

shortOfficerMove = do
    t <- officerType
    m <- moveType
    s <- square
    let e = PGNMoveEssentials Nothing m s
    return $ OfficerMove t e

longOfficerMove = do
    t <- officerType
    h <- officerHint
    m <- moveType
    s <- square
    let e = PGNMoveEssentials (Just h) m s
    return $ OfficerMove t e

pgnMove = choice [try pawnMove, try officerMove, castles]

