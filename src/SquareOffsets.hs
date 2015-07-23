module SquareOffsets (offset) where
import Directions
import Square

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
