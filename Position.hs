module Position where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Control.Monad.Reader

import MoveTypes
import Piece
import Square

data Position = Position

type PReader = Reader Position

legal :: Position -> Bool
legal _ = True

turn :: PReader Color
turn = return White -- TODO


filterPieces :: Piece -> PReader [Square]
filterPieces pc = do
    sqs <- pieceSquares
    filterM (hasPiece pc) sqs

hasPiece :: Piece -> Square -> PReader Bool
hasPiece pc sq = do
    mpc <- pieceAt sq
    return (mpc == Just pc)

pieceAt :: Square -> PReader (Maybe Piece)
pieceAt sq = return Nothing

pieceSquares :: PReader [Square] -- TODO
pieceSquares = return []
