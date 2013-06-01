module Board (Board, readSquare, (!!!), squareRead, put, remove, move) where
import Control.Arrow ((&&&))
import Control.Monad.State (State, StateT(..))
import Data.Maybe (fromJust)

import Square ((!!!), Square)
import Piece (Piece)

type Board = [Maybe Piece] 

readSquare :: Board -> Square -> Maybe Piece
readSquare = (!!!)

squareRead = flip readSquare

-- TODO test this function
replace :: Square -> Maybe Piece -> Board -> Board
replace s mp b = let (part1, part2) = splitAt (fromEnum s) b
                  in part1 ++ mp : drop 1 part2

clear s = replace s Nothing

type BoardState = State Board

noSideEffect :: (b -> c) -> b -> ((), c)
noSideEffect f = const () &&& f

stateify = StateT . returnResult 
    where returnResult = (return.)

putMaybe s mp = stateify $ noSideEffect $ replace s mp

put :: Piece -> Square -> BoardState ()
put p s = putMaybe s (Just p)

remove :: Square -> BoardState (Maybe Piece)
remove s = stateify $ squareRead s  &&& clear s

move s d = do
    mp <- remove s
    put (fromJust mp) s
