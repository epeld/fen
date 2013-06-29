{-#LANGUAGE NoMonomorphismRestriction #-}
module FEN where

import Text.Parsec
import Data.Char
import Data.List
import Piece
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Maybe
import Board
import qualified Square
import Color
import CastlingSide
import CastlingRight
import qualified Position

startingPosition = case runParser position 0 "FEN-String" startingPosFen of
    Right p -> p
    _ -> error "Couldn't parse FEN"

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

fenSquares = [Square.square' f r | r <- [8,7..1], f <- ['a'..'h']]

board = concat <$> reverse <$> sepBy1 row (char '/')
position = do
    b <- board
    space
    m <- whoseMove
    space
    c <- castlingRights
    space
    e <- enPassant
    space
    h <- halfMove
    space
    f <- fullMove
    return $ Position.Position {
        Position.board = b,
        Position.whoseTurn = m,
        Position.enPassant = e,
        Position.castlingRights = c,
        Position.fullMoves = f,
        Position.halfMoves = h
        }

white = char 'w' >> return White
black = char 'b' >> return Black
omitted = char '-' >> return (fail "-")
digits = many1 digit >>= return . read

whoseMove = white <|> black <?> "color"
castlingRights = many1 castlingRight <|> omitted <?> "castling rights"
enPassant = fmap Just square <|> omitted <?> "passant-square"
halfMove = digits <?> "half-move number"
fullMove = digits <?> "move-number"

square = do
    f <- oneOf "abcdefgh" <?> "file"
    r <- liftM digitToInt (oneOf "12345678" <?> "rank")
    return $ Square.square' f r


castlingSide c =
    case toLower c of
        'k' -> Kingside
        'q' -> Queenside

castlingRight = do
    c <- oneOf "KkQq"
    return $ CastlingRight.Castles (castlingSide c) (charToColor c)
