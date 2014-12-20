module Position where
import Prelude ()
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Map
import Data.Function

import Text.Show
import Control.Monad.Reader

import MoveType
import Piece
import Square

type Board = Map Square Piece

data Position = Position {
    board :: Board,
    turn :: Color
    } deriving (Show)

