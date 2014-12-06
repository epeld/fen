module MoveUtils where
import Prelude ()
import Control.Monad
import Data.Function

import MoveTypes
import Position
import Piece

pieceType :: Move -> PieceType
pieceType _ = PieceType -- TODO

piece :: Move -> PReader Piece
piece mv = do
    clr <- Position.turn
    return $ Piece (pieceType mv) clr
