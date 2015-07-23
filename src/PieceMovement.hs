module PieceMovement where
import PawnMovement
import OfficerMovement
import Movement
import Square
import Piece
import MoveType

movementFn :: MoveType -> Piece -> MovementFn
movementFn mt (Piece Pawn color) = PawnMovement.movementFn mt color
movementFn _ (Officer ot _) = OfficerMovement.movementFn ot
