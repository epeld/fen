module ChessMove (ChessMove(..)) where
import CastlingRight (Side)
import Move (Move)
import Position (Position)
import Castles (CastlingMove)

data ChessMove = Either Move CastlingMove 
