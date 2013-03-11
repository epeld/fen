module Board where
import Data.Array.IArray
import Piece
import Square
import Data.List(find)

type Board = [Maybe Piece] 

findPieceSquare pc b =
    let criterion sq = maybe False (==pc) (b !!! sq)
     in
        find criterion b
