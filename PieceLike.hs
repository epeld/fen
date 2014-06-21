module PieceLike where
import qualified Piece
import qualified Square
import qualified Position

class PieceLike a where
  piece :: a -> Piece.Piece
  square :: a -> Square.Square
  position :: a -> Position.Position

pieceType :: PieceLike p => p -> Piece.PieceType
pieceType = pieceType. boundPiece

color :: PieceLike p => p -> Piece.PieceType
color = color. piece

isFriendly :: PieceLike p => p -> Bool
isFriendly p = turn (position p) == color p

isPawn :: PieceLike p => p -> Bool
isPawn p = pieceType p == Pawn

isInitialRank :: PieceLike p => p -> Bool
isInitialRank p = initialRank (color p) == rank (square p)

forward :: PieceLike p => p -> Stepper
forward p = Stepping.forward. color
