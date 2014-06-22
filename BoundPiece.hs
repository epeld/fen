module BoundPiece where
import Color (Color)
import PieceLike (PieceLike(..))
import qualified Position
import qualified Piece as UnboundPiece
import qualified Square

data Piece = Piece Square.Square Position.Position

instance PieceLike BoundPiece.Piece where
  piece (BoundPiece.Piece sq p) = case Position.lookup sq p of
    Nothing -> error "incorrectly bound piece"
    Just pc -> pc

  square (Piece sq _) = sq
  position (Piece _ p) = p

bind :: Square.Square -> Position.Position -> Maybe BoundPiece.Piece
bind sq p = if Position.isEmpty p sq
            then Nothing
            else Just (Piece sq p)
