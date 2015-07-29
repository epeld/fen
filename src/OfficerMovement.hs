module OfficerMovement where
import Square
import ListUtils
import Piece
import Directions
import SquareOffsets
import Movement


movementFn :: OfficerType -> MovementFn
movementFn ot = rangeFn ot (directions ot)


rangeFn :: OfficerType -> RangeFn
rangeFn King = shortRange
rangeFn Knight = shortRange
rangeFn _ = longRange

