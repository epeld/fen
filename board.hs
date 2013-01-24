module Board where
import Data.Array.IArray
import Piece
import Square

data Board = Board [Maybe Piece] deriving Show
