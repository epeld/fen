module Chess where
import Control.Applicative
import Control.Monad
import Data.Maybe
import Square
import Game
import Piece

--TODO REMEMBER PROMOTIONS!

makeMove g s d = 
    let b = updateBoard s d g
        p = updateProperties s d g
     in 
       Game g s d

isValidMove g s d =
    let g' = makeMove g s d
     in
        isValidPosition g' && isReachable s d g


isReachable s d g = False
