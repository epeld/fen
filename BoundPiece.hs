module BoundPiece where
import Color (Color)
import PieceLike (PieceLike)
import qualified Position
import qualified Piece

data Piece = Piece Square Position

instance PieceLike Piece where
  piece (Piece sq p) = case Position.lookup sq p of
    Nothing -> error "incorrectly bound piece"
    Just pc -> pc

  square (Piece sq _) = sq
  position (Piece _ p) = p

bind :: Square -> Position -> Maybe Piece
bind sq p = if isEmpty sq p
            then Nothing
            else Just (Piece sq p)
