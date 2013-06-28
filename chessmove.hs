module ChessMove (ChessMove(..), whose, whoseTurnAfter, ChessMove.board, position) where
import CastlingRight 
import Position 
import Color
import qualified Castles 
import qualified Move 
import Move (Move)
import Castles (CastlingMove)

data ChessMove = Either Move CastlingMove 

whose (Left mv) = Move.whose mv
whose (Right mv) = Castles.whose mv

whoseTurnAfter = invert. whose

position (Right mv) = Castles.position mv
position (Left mv) = Move.position mv

board = Position.board. position
