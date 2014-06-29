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
      isCapturable sq = Position.isEnemyAt pos sq ||
                        (PieceLike.isPawn p && Position.isPassant pos sq)
  in take 1 $ List.filter isCapturable $ dropWhile (Position.isEmpty pos) seq

applySeq p Moves seq = takeWhile (Position.isEmpty $ PieceLike.position p) seq
