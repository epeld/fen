module Chess where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Arrow hiding (left,right)
import Data.List
import Data.Maybe
import Square
import Game
import Piece
import MonadOps

data Move = Move Square Square

makeMove (Game b p) (Move s d) = do
    let maybePiece = b !!! d
    when (isNothing maybePiece) $ fail "No piece to move"
    piece <- maybePiece
    let Piece t c = piece
    let reachables = reachableSquaresByOfficerAt t s
    unless (d `elem` concat reachables) $ fail "Cannot reach destination"
    dir <- find (elem d) reachables
    ix <- findIndex (d ==) dir
    let isOccupied = not. isNothing
    hit <- maybe (length dir) (findIndex isOccupied) dir
    --TODO check for same-colored piece
    unless (ix <= hit) $ fail "Cannot reach destination"
    --TODO replace

reachableSquaresByOfficerAt Rook s  = sequence' s <$> [up 1, left 1, down 1, right 1]
reachableSquaresByOfficerAt Queen s = concat . flip reachableSquaresByOfficerAt s <$>
    [Rook, Bishop]
reachableSquaresByOfficerAt King s  = take 1 <$> reachableSquaresByOfficerAt Queen s
reachableSquaresByOfficerAt Knight s = fromSquare' s <$> knightJumps

knightJumps = 
    let long = [up, right] <*> [2,-2]
        short = [right, up] <*> [1,-1]
     in
        zipWith (>=>) long short

relUp (Game _ p) = Square.relUp 1 (whoseMove p)

