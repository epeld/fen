module Range where
import qualified Data.List as List

import Theoretical (MoveType(..))
import qualified Theoretical
import qualified Position
import qualified Square
import qualified PieceLike

type Range = [Square.Square]

range :: PieceLike.PieceLike p =>
         p -> Theoretical.MoveType -> Range
range p mt = let theory = Theoretical.range p mt
             in applied p mt theory

applied :: PieceLike.PieceLike p =>
           p -> Theoretical.MoveType -> Theoretical.Range -> Range
applied p mt = concat. map (applySeq p mt)

applySeq :: PieceLike.PieceLike p =>
            p -> Theoretical.MoveType -> Theoretical.Sequence -> [Square.Square]
applySeq p Takes seq =
  let pos = PieceLike.position p
      enemies = filter (Position.isEnemyAt pos) seq
      passant = filter (Position.isPassant pos) seq
  in if PieceLike.isPawn p
     then take 1 $ enemies ++ passant
     else take 1 enemies

applySeq p Moves seq = takeWhile (Position.isEmpty $ PieceLike.position p) seq
