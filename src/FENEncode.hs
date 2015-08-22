module FENEncode where
import Control.Lens
import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split
import Data.List
import Data.Char
import Text.Show

import qualified Position
import Position (Position)
import Square
import Piece
import Castling

type FENString = String


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

fen :: Position -> FENString
fen = separated [Position.board `views` board, properties]


board :: Position.Board -> String
board = concat . intersperse "/" . rows 
    where   
    rows b = fmap (`rle` b) (chunksOf 8 fenSquares)


properties :: Position -> String
properties = separated [turn, castlingRights, passant, halfMove, fullMove]
    where
    halfMove = Position.halfMoveCount `views` show
    fullMove = Position.fullMoveCount `views` show


-- Run-length encode the contents of the given sequence of squares
rle :: [Square] -> Position.Board -> String
rle sqs b = concatMap enc groups
    where
    groups = NonEmpty.group (fmap contentOf sqs)

    enc grp = case grp of
        Nothing :| _ -> show (NonEmpty.length grp)
        Just p :| _ -> NonEmpty.toList grp & mapped .~ piece p

    contentOf sq = b ^. at sq


-- An enumeration of all the chess squares in the order they appear in the FEN string
fenSquares :: [Square]
fenSquares = do
    r <- reverse [1 .. 8]
    f <- [1 .. 8]
    return $ Square (f, r) 
    

turn :: Position -> String
turn = color . view Position.turn
    where
    color White = "w"
    color Black = "b"


castlingRights :: Position -> String
castlingRights p = nonEmpty (rightsString p)
    where 
    nonEmpty "" = "-"
    nonEmpty x = x

    rightsString = fmap enc . Set.toDescList . view Position.castlingRights

    enc (Castling White Kingside) = 'K'
    enc (Castling White Queenside) = 'Q'
    enc (Castling Black Kingside) = 'k'
    enc (Castling Black Queenside) = 'q'


passant :: Position -> String
passant p = case p ^. Position.passant of
    Nothing -> "-"
    Just sq -> Square.string sq


piece :: Piece -> Char
piece (Piece pt c) = cased c (pieceType pt)
    where
    cased White = toUpper
    cased Black = toLower

    pieceType Pawn = 'p'
    pieceType (Officer ot) = enc ot
        where
        enc Bishop = 'b'
        enc Rook = 'r'
        enc Knight = 'n'
        enc King = 'k'
        enc Queen = 'q'


separated :: [Position -> String] -> Position -> String
separated parsers = unwords . sequence parsers
