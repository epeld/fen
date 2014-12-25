module UpdateFunctions where
import Prelude (undefined)
import Data.Eq
import Data.Function
import Data.Map
import Data.Maybe
import Data.Bool
import Text.Show
import Data.List ((++))
import Control.Monad
import Control.Monad.Reader

import ListUtils
import MoveDescription
import qualified PartialDescription as Partial
import Square
import Piece
import MoveType
import qualified Position
import Position (Position)
import PositionReader
import FullDescription

-- Represents the information needed to create a new position from an old
-- e.g we need to know how the position looked initially, and we need to know what is going to change
data UpdateEnvironment = UpdateEnvironment { move :: FullMove, originalPosition :: Position }

type UpdateReader = Reader UpdateEnvironment

-- We will create a new position from an old one by generating lots of small update functions,
-- each updating a single property of the position, and then composing them
type UpdateFn = Position -> Position

