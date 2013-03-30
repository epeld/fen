module ProjectedRange where
import qualified MovingPiece (
    MovingPiece,
    range,
    position,
    pieceType,
    )
import Range (
    Range,
    )
import Position (Position)
import MoveType (MoveType)

data ProjectedRange = Projected {
    range :: Range,
    position :: Position
    } deriving (Show)

projectedRange :: MovingPiece.MovingPiece -> MoveType -> ProjectedRange
projectedRange mp mt =
    MovingPiece.range mp mt `project` MovingPiece.position mp

project r p = Projected r p
