module MoveUtils where
import Prelude ()
import Control.Monad
import Data.Function

import Square
import MoveTypes
import Position
import Piece

piece :: Move -> PReader Piece
piece mv = do
    clr <- Position.turn
    return $ Piece (pieceType mv) clr


findPieces :: Move -> PReader [Square]
findPieces = piece >=> filterPieces
