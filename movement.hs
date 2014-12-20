module Movement where
import Prelude (Enum)
import Data.Maybe
import Data.Functor
import Data.List
import Data.Int
import Data.Eq
import Text.Show

import Square (Square, Offset, add)
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

seq :: Square -> Direction -> [Square]
seq sq d = 
    case add sq (offset d) of
        Just sq' -> (sq' : seq sq' d)
        Nothing -> []

apply :: Square -> [Direction] -> [[Square]]
apply sq ds = fmap (seq sq) ds

applyN :: Square -> [Direction] -> Int -> [[Square]]
applyN sq ds n = fmap (take n) (apply sq ds)

apply1 :: Square -> [Direction] -> [[Square]]
apply1 sq ds = applyN sq ds 1

offset :: Direction -> Offset
offset (Straight s) = soffset s
offset (Diagonal d) = doffset d
offset (LShaped k) = koffset k

soffset :: StraightDirection -> Offset
soffset (Vertical v) = voffset v
soffset (Horizontal h) = hoffset h

voffset :: VerticalDirection -> Offset
voffset North = (0, -1)
voffset South = (0, 1)

hoffset :: HorizontalDirection -> Offset
hoffset West = (-1, 0)
hoffset East = (1, 0)

koffset :: KnightsJump -> Offset
koffset OneOClock = (1,2)
koffset TwoOClock = (2,1)
koffset FourOClock = (2,-1)
koffset FiveOClock = (1,-2)
koffset SevenOClock = (-1,-2)
koffset EightOClock = (-2,-1)
koffset TenOClock = (-2,1)
koffset ElevenOClock = (-2,-1)

doffset :: DiagonalDirection -> Offset
doffset NorthWest = (-1, 1)
doffset NorthEast = (1,1)
doffset SouthEast = (1,-1)
doffset SouthWest = (-1, -1)
