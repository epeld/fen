module PgnMove where

import Castling
import PartialMove

newtype PgnMove = Either Castling.Side PartialMove
