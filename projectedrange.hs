module ProjectedRange where
import MovingPiece (MovingPiece)
import Range (Range)
import Internals (LegalPosition)

data ProjectedRange = Projected {
    range :: Range,
    position :: LegalPosition
    } deriving (Show)

range mp = Range.range (color mp) (pieceType mp) (square mp)

projectedRange :: MovingPiece -> ProjectedRange
projectedRange mp = pieceType mp 
