module Position where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Map
import Data.Function

import Text.Show
import Control.Monad.Reader

import MoveTypes
import Piece
import Square

type Board = Map Square Piece

data Position = Position Board Color deriving (Show)

type PReader = Reader Position

legal :: Position -> Bool
legal _ = True -- TODO


filterPieces :: Piece -> PReader [Square]
filterPieces pc = do
    sqs <- pieceSquares
    filterM (hasPiece pc) sqs

hasPiece :: Piece -> Square -> PReader Bool
hasPiece pc sq = do
    mpc <- pieceAt sq
    return (mpc == Just pc)

pieceAt :: Square -> PReader (Maybe Piece)
pieceAt sq = fmap (lookup sq) board

pieceSquares :: PReader [Square]
pieceSquares = fmap keys board

--
-- Accessors
--

board :: PReader Board
board = do
    Position b _ <- ask
    return b

turn :: PReader Color
turn = do
    Position _ c <- ask
    return c

