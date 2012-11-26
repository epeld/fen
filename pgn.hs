{-#LANGUAGE NoMonomorphismRestriction #-}
module Pgn where

import Square hiding (file, rank)
import Game
import Chess
import Piece
import Data.Char
import Text.Parsec

data PGNMove = PawnMove (Maybe Hint) MoveType Square (Maybe OfficerType) |
               OfficerMove OfficerType (Maybe Hint) MoveType Square |
               Castles Side deriving (Show)

data Hint = FileHint File | RankHint Rank | SquareHint Square deriving Show

castlesKingside = string "O-O" >> return (Castles Kingside)
castlesQueenside = string "O-O-O" >> return (Castles Queenside)

castles = try castlesQueenside <|> castlesKingside

moveType = option Moves (char 'x' >> return Takes) 

longPawnMove = do
    h <- pawnHint
    m <- moveType
    s <- square
    p <- optionMaybe promotion
    return $ PawnMove (Just h) m s p

shortPawnMove = do
    s <- square
    p <- optionMaybe promotion
    return $ PawnMove Nothing Moves s p

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
    return $ OfficerMove t Nothing m s

longOfficerMove = do
    t <- officerType
    h <- officerHint
    m <- moveType
    s <- square
    return $ OfficerMove t (Just h) m s

pgnMove = choice [try pawnMove, try officerMove, castles]

resetHalfMoves p =
    let c = whoseMove p
        r = castlingRights p
        e = enPassantSquare p
        m = moveNumber p
    GameProperties $ c r e 0 m


doPawnMove g mv@(PawnMove h m s p) =
    do  src <- candidate Pawn m s h
        doNaturalPawnMove g src m s p

matchHint h s =
    case h of
        FileHint f -> f == file s
        RankHint r -> r == rank s
        SquareHint s2 -> s2 == s
        _ -> True

data CandidateError = NoSuitableCandidates | TooManyCandidates
candidate g t m s h = 
    case take 1 $ filter matchHint $ candidates g t m s of
        [x] -> return x
        []  -> fail NoSuitableCandidates
        _   -> fail TooManyCandidates

assertCanDoPawnMove g (PawnMove h m s p) =
    case m of
        Takes -> assertCanPawnTake g s
        Moves -> assertCanPawnMove g s

assertCanPawnTake g s = 
    case pieceAt g s of
        Just (Piece p c) -> if c /= whoseMove g
            then return ()
            else fail SameColorCapture
        Nothing -> if passantSquare g == Just s
            then return ()
            else fail NothingToCapture

assertCanPawnMove g s =
    case pieceAt g s of
        Nothing -> return ()
        _ -> fail SquareOccupied

whoseMove = Game.whoseMove . props 
enPassant = enPassantSquare . props
