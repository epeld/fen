module ChessMove (ChessMove(..), castles, move) where
import CastlingRight (Side)
import Move (Move)
import MoveLogic (move)
import Position (Position)
import Castles (castles, CastlingMove)

data ChessMove = Either Move CastlingMove 
