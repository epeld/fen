module PositionReader where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Int
import Data.Map
import Data.Function
import Data.Monoid

import Text.Show
import Control.Monad.Reader
import Control.Applicative

import qualified Position as P
import MoveType
import Piece
import Square
import Movement

type PReader = Reader P.Position

kingSquares :: Color -> PReader [Square]
kingSquares c = filterPieces king
    where king = Piece (Officer King) c

filterPieces :: Piece -> PReader [Square]
filterPieces pc = do
    sqs <- pieceSquares
    filterM (hasPiece pc) sqs

hasPiece :: Piece -> Square -> PReader Bool
hasPiece pc sq = do
    mpc <- pieceAt sq
    return (mpc == Just pc)

behind :: Square -> PReader (Maybe Square)
behind sq = do
    color <- turn
    return $ PawnMovement.behind sq color

--
-- Accessors
--

lastRank :: PReader Int
lastRank = fmap P.lastRank turn

pieceAt :: Square -> PReader (Maybe Piece)
pieceAt sq = boardAccessor (lookup sq)

emptyAt :: Square -> PReader Bool
emptyAt sq = boardAccessor (notMember sq)

occupiedAt :: Square -> PReader Bool
occupiedAt sq = boardAccessor (member sq)


pieceSquares :: PReader [Square]
pieceSquares = boardAccessor keys

board :: PReader P.Board
board = accessor P.board

turn :: PReader Color
turn = accessor P.turn

--
-- Definitions

accessor :: (P.Position -> a) -> PReader a
boardAccessor :: (P.Board -> a) -> PReader a

accessor f = fmap f ask
boardAccessor f = accessor (f. P.board)
