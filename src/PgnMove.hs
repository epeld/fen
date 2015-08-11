module PgnMove where

import Castling
import PartialMove

type PgnMove = Either Castling.Side PartialMove
