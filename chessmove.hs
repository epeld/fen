module ChessMove (ChessMove(..)) where
import CastlingRight (Side)
import Move (Move)

data ChessMove = Standard Move | Castles Side --deriving (Show, Eq)
