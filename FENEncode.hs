module FENEncode where
import Prelude (String)

import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Map
import Data.List.Split
import qualified Data.Set as Set
import Data.List (unwords, concatMap, group, replicate, length, head, sort, intersperse)
import Control.Applicative
import Control.Arrow
import Data.Char
import Text.Show

import Position
import Square
import Piece
import Castling

{-
Here is the FEN for the starting position:

rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
Here is the FEN after the move 1. e4:

rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
And then after 1. ... c5:

rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
And then after 2. Nf3:

rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
-}

encode :: Position -> String
encode = mconcat [encodeBoard. board, encodeProperties]

encodeBoard :: Board -> String
encodeBoard b = mconcat $ intersperse "/" encodedRows
    where   
    encodedRows = encodeRow <$> chunksOf 8 fenSquares <*> pure b

encodeRow :: [Square] -> Board -> String
encodeRow sqs b =
    let pcs = fmap (flip lookup b) sqs
        enc (i, Nothing) = [intToDigit i]
        enc (i, Just p) = replicate i (encodePiece p)
        histo = length &&& head -- group guarantees head is safe
    in concatMap (enc. histo) (group pcs)

fenSquares :: [Square]
fenSquares = let sq a b = Square (b, a) in sq <$> [8,7..1] <*> [1..8]

encodeProperties :: Position -> String
encodeProperties = mconcat [encodeTurn, encodeCastlingRights, encodePassant, encodeHalfMove, encodeFullMove]


encodeTurn :: Position -> String
encodeTurn p = s (turn p)
    where s White = "w"
          s Black = "b"

encodeCastlingRights :: Position -> String
encodeCastlingRights = sort. fmap encodeRight. Set.toList. castlingRights
    where encodeRight cr@(Castling clr side) = cased clr (encodeSide side)
          encodeSide Kingside = 'k'
          encodeSide Queenside = 'q'

encodePassant :: Position -> String
encodePassant p = s (passant p)
    where s Nothing = "-"
          s (Just sq) = Square.string sq

encodeFullMove :: Position -> String
encodeFullMove p = show (fullMoveCount p)

encodeHalfMove :: Position -> String
encodeHalfMove p = show (halfMoveCount p)




encodePiece :: Piece -> Char
encodePiece (Piece pt c) = cased c (encodePieceType pt)

type CaseFn = Char -> Char

cased :: Color -> CaseFn
cased White = toUpper
cased Black = toLower

encodePieceType :: PieceType -> Char
encodePieceType Pawn = 'p'
encodePieceType (Officer ot) = enc ot
    where   enc Bishop = 'b'
            enc Rook = 'r'
            enc Knight = 'n'
            enc King = 'k'
            enc Queen = 'q'
