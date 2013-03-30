module Board where
import Square ((!!!), Square)
import Piece (Piece)

type Board = [Maybe Piece] 

readSquare :: Board -> Square -> Maybe Piece
readSquare = (!!!)
