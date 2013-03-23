module Position where

import Piece
import Board
import Internals

readSquare p s = Board.readSquare (board p) s
