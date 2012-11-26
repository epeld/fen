module Board where
import Data.Array.IArray
import Piece
import Square

data Board = Board (Array Square (Maybe Piece)) deriving Show

pieceAt :: Square -> Board -> Maybe Piece
pieceAt s (Board a) = a ! s

board :: [(Square, Maybe Piece)] -> Board
board e = let a1 = square' "a1"
              h8 = square' "h8"
           in Board $ array (a1,h8) e

putPiece = putMaybePiece . Just
removePiece = putMaybePiece Nothing
putMaybePiece p s b = board $ assocs b `map` $ \(k,v) ->
    if k == s then p else v

movePiece src d b = 
    let p = pieceAt b src
     in putPiece p d . removePiece src $ b
