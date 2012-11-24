module Board where
import Data.Array.IArray
import Piece
import Square

data Board = Board (Array Square (Maybe Piece)) deriving Show

pieceAt :: Board -> Square -> Maybe Piece
pieceAt (Board a) s = a ! s

board :: [(Square, Maybe Piece)] -> Board
board e = let a1 = square' "a1"
              h8 = square' "h8"
           in Board $ array (a1,h8) e
