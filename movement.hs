module Movement where
import Data.Maybe

import Square (Square, Offset)

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

diagonalSquares :: Square -> [DiagonalDirection] -> [Square]
diagonalSquares sq ds = mapMaybe (add sq) (fmap doffset ds)


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

pawnAttackDiagonals :: Color -> [DiagonalDirection]
pawnAttackDiagonals White = [NorthWest, NorthEast]
pawnAttackDiagonals Black = [SouthWest, SouthEast]

