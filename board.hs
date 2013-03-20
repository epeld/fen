module Board where
import Data.Array.IArray
import Piece
import Square
import Data.List(find)

type Board = [Maybe Piece] 

