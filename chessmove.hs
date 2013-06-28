module ChessMove (ChessMove(..), whose, whoseTurnAfter, ChessMove.board, position) where
import Data.Either 

import CastlingRight 
import Position 
import Color
import qualified Castles 
import qualified Move 

data ChessMove = Either Move.Move Castles.CastlingMove 

whose (Left mv) = Move.whose mv
whose (Right mv) = Castles.whose mv

whoseTurnAfter = invert. whose

position (Right mv) = Castles.position mv
position (Left mv) = Move.position mv

board = Position.board. position
