module PositionReader where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Map
import Data.Function
import Data.Monoid

import Text.Show
import Control.Monad.Reader
import Control.Applicative

import qualified Position as P
import MoveTypes
import Piece
import Square

type PReader = Reader P.Position

allAssailants :: Color -> Square -> PReader [Square]
allAssailants c sq = do
    pieces <- Piece <$> [Pawn ..] <*> [c]
    as <- sequence $ assailants <$> pieces <*> [sq]
    return (mconcat as)

assailants :: Piece -> Square -> PReader [Square]
assailants pawn@(Piece Pawn c) sq = 
    let fromDirs = pawnAttackDiagonals $ otherColor c
     in filterM (hasPiece pawn) (diagonalSquares sq fromDirs)

assailants knight@(Piece (Officer Knight) c) sq = filterM (hasPiece knight) (knightSquares sq)
assailants piece sq = firstPiece piece sq (movePattern piece)

kingSquares :: Color -> PReader [Square]
kingSquares c = filterPieces (Piece (Officer King) c)

filterPieces :: Piece -> PReader [Square]
filterPieces pc = do
    sqs <- pieceSquares
    filterM (hasPiece pc) sqs

hasPiece :: Piece -> Square -> PReader Bool
hasPiece pc sq = do
    mpc <- pieceAt sq
    return (mpc == Just pc)

--
-- Accessors
--

pieceAt :: Square -> PReader (Maybe Piece)
pieceAt sq = boardAccessor (lookup sq)

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
