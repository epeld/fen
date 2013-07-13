module Board (Board, readSquare, (!!!), squareRead, put, remove, move,
              pieceSquares, colorSquares,) where
import Control.Arrow ((&&&))
import Control.Monad (liftM)
import Control.Monad.State (State, StateT(..))
import Data.Maybe (fromJust)
import Data.List (findIndices)

import Square ((!!!), Square)
import Piece (Piece, hasColor)
import Color (Color)

type Board = [Maybe Piece] 

readSquare :: Board -> Square -> Maybe Piece
readSquare = (!!!)

pieceSquares :: Piece -> Board -> [Square]
pieceSquares pc = liftM toEnum . findIndices (Just pc ==)

colorSquares :: Color -> Board -> [Square]
colorSquares c = liftM toEnum . findIndices (maybe False $ hasColor c)

squareRead = flip readSquare

-- TODO test this function
replace :: Square -> Maybe Piece -> Board -> Board
replace s mp b = let (part1, part2) = splitAt (fromEnum s) b
                  in part1 ++ mp : drop 1 part2

clear s = replace s Nothing

type BoardState = State Board

noReturn :: (b -> c) -> b -> ((), c)
noReturn f = const () &&& f

stateify = StateT . returnResult 
    where returnResult = (return.)

putMaybe s mp = stateify $ noReturn $ replace s mp

put :: Piece -> Square -> BoardState ()
put p s = putMaybe s (Just p)

remove :: Square -> BoardState (Maybe Piece)
remove s = stateify $ squareRead s  &&& clear s

move s d = do
    mp <- remove s
    put (fromJust mp) s
