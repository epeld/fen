{-#LANGUAGE NoMonomorphismRestriction #-}
module PGNParse where
import Control.Applicative ((<$>))

import PGNSquare (square, file, rank)
import Square hiding (file, rank, square, square', string)
import Piece (Piece(..), OfficerType(..), PieceType(..), charToOfficerType)
import CastlingSide
import Data.Char
import Text.Parsec
import MoveType
import qualified PGNMove
import qualified PGNMove
import qualified PGNMoveEssentials

castlesKingside = string "O-O" >> return (PGNMove.Castles Kingside)
castlesQueenside = string "O-O-O" >> return (PGNMove.Castles Queenside)

castles = try castlesQueenside <|> castlesKingside

moveType = option Moves (char 'x' >> return Takes) 

-- "exd4"
longPawnMove = do
    h <- pawnHint
    m <- moveType
    s <- square
    p <- optionMaybe promotion
    let e = PGNMoveEssentials.Essentials {
        PGNMoveEssentials.hint = Just h,
        PGNMoveEssentials.moveType = m,
        PGNMoveEssentials.destination = s
        }
    return $ PGNMove.PawnMove e p

-- "e4=D"
shortPawnMove = do
    s <- square
    p <- optionMaybe promotion
    let e = PGNMoveEssentials.Essentials {
        PGNMoveEssentials.hint = Nothing,
        PGNMoveEssentials.moveType = Moves,
        PGNMoveEssentials.destination = s
        }
    return $ PGNMove.PawnMove e p

promotion = string "=" >> charToOfficerType <$> oneOf "RBQN"

squareHint = PGNMoveEssentials.SquareHint <$> square 
fileHint = PGNMoveEssentials.FileHint <$> file 
rankHint = PGNMoveEssentials.RankHint <$> rank 

officerHint = try rankHint <|> pawnHint
pawnHint = try squareHint <|> fileHint

pawnMove = choice [try longPawnMove, shortPawnMove]

officerType = charToOfficerType <$> oneOf "RKNQB"

officerMove = choice [try longOfficerMove, shortOfficerMove]

shortOfficerMove = do
    t <- officerType
    m <- moveType
    s <- square
    let e = PGNMoveEssentials.Essentials Nothing m s
    return $ PGNMove.OfficerMove t e

longOfficerMove = do
    t <- officerType
    h <- officerHint
    m <- moveType
    s <- square
    let e = PGNMoveEssentials.Essentials (Just h) m s
    return $ PGNMove.OfficerMove t e

pgnMove = choice [try pawnMove, try officerMove, castles]
