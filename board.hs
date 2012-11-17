module Board where
import Data.Array.IArray
import Piece
import Square

data Board = Board (Array Square (Maybe Piece))
