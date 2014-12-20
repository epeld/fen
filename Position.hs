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

hasPiece :: Position -> Piece -> Square -> Bool
hasPiece pos pc sq = pieceAt pos sq == Just pc

pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos sq = lookup sq (board pos)

