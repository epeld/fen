{-#LANGUAGE NoMonomorphismRestriction #-}
module Fen where

import Text.Parsec
import Data.Char
import Data.List
import Piece
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Maybe
import Data.Array.IArray
import Board
import Square
import qualified Game

startingPosFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

pieceChars :: [Char]
pieceChars = let blacks = nub . takeWhile (/= '/') $ startingPosFen
              in blacks ++ map toUpper blacks ++ "pP"
    
charToPieceType c = case toLower c of
    'p' -> Pawn
    _ -> Officer $ charToOfficerType c

charToColor c = case isUpper c of
    True -> White
    False -> Black

charToPiece c = Piece (charToPieceType c) (charToColor c)

-- pieceChars = "NBRKQ"
piece = oneOf pieceChars >>= return . Just . charToPiece
pieces = many1 piece <?> "piece char"

invalidFenLength l = "Invalid FEN row: not all squares specified " ++ show l

verifyLength8 l = unless (l == 8) (fail $ invalidFenLength l)

rleSpace = digit >>= return . flip replicate Nothing . digitToInt
row = do
    all <- concat <$> many1 (pieces <|> rleSpace)
    verifyLength8 (length all)
    return all

fenSquares = [Square (File f) (Rank r) | r <- [8,7..1], f <- ['a'..'h']]

board = concat <$> reverse <$> sepBy1 row (char '/')
props = do
    m <- whoseMove
    space
    c <- castlingRights
    space
    e <- enPassant
    space
    h <- halfMove
    space
    f <- fullMove
    return $ Game.GameProperties m c e h f

white = char 'w' >> return White
black = char 'b' >> return Black
omitted = char '-' >> return (fail "-")
digits = many1 digit >>= return . read

whoseMove = white <|> black <?> "color"
castlingRights = many1 castlingRight <|> omitted <?> "castling rights"
enPassant = fmap Just square <|> omitted <?> "passant-square"
halfMove = digits <?> "half move number"
fullMove = digits <?> "move number"

castlingSide c =
    case toLower c of
        'k' -> Game.Kingside
        'q' -> Game.Queenside

castlingRight = do
    c <- oneOf "KkQq"
    return $ Game.CastlingRight (castlingSide c) (charToColor c)

game = do
    b <- Fen.board
    space
    p <- props
    return $ Game.Game b p
