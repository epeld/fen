module Directions where
import Prelude (Enum)
import Data.Maybe
import Data.Functor
import Data.Function
import Data.List
import Data.Int
import Data.Eq
import Text.Show

import Square (Square, Offset)
import Piece

data KnightsJump = OneOClock | TwoOClock | FourOClock | FiveOClock |
                   SevenOClock | EightOClock | TenOClock | ElevenOClock
                   deriving (Show, Eq, Enum)

data DiagonalDirection = NorthWest | NorthEast | SouthWest | SouthEast 
                         deriving (Show, Eq, Enum)

data VerticalDirection = North | South
                         deriving (Show, Eq, Enum)

data HorizontalDirection = West | East
                           deriving (Show, Eq, Enum)

data StraightDirection = Vertical VerticalDirection | Horizontal HorizontalDirection
                         deriving (Show, Eq)

data Direction = Diagonal DiagonalDirection | LShaped KnightsJump | Straight StraightDirection
                 deriving (Show, Eq)

directions :: OfficerType -> [Direction]
directions Bishop = fmap Diagonal [NorthWest .. SouthEast]
directions Rook = fmap Straight (fmap Horizontal [West, East] ++ fmap Vertical [North, South])
directions Knight = fmap LShaped [OneOClock .. ElevenOClock ]
directions Queen = directions Bishop ++ directions Rook
directions King = directions Queen

pawnAttackSources :: Color -> [Direction]
pawnAttackSources White = fmap Diagonal [SouthWest, SouthEast]
pawnAttackSources Black = fmap Diagonal [NorthWest, NorthEast]
