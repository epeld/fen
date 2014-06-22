module PieceLike where
import qualified Piece
import qualified Square
import qualified Position
import qualified Stepping
import qualified Color

class PieceLike a where
  piece :: a -> Piece.Piece
  square :: a -> Square.Square
  position :: a -> Position.Position

pieceType :: PieceLike p => p -> Piece.PieceType
pieceType = Piece.pieceType. piece

color :: PieceLike p => p -> Color.Color
color = Piece.color. piece

isFriendly :: PieceLike p => p -> Bool
isFriendly p = Position.turn (position p) == color p

isPawn :: PieceLike p => p -> Bool
isPawn p = pieceType p == Piece.Pawn

isInitialRank :: PieceLike p => p -> Bool
isInitialRank p = Color.initialRank (color p) == Square.rank (square p)

forward :: PieceLike p => p -> Stepping.Stepper
forward = Stepping.forward. color
