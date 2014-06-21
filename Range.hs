module Range where
import Data.Set

import qualified Theoretical


type Range = Data.Set Square

range :: PieceLike p => p -> Theoretical.MoveType -> Range
range p mt = let theory = Theoretical.range p mt
             in applied mt theory

applied :: PieceLike p =>
           p -> Theoretical.MoveType -> Theoretical.Range -> Range
applied p mt seqs = Data.Set.fromList. concat. map (applySeq p mt) seqs

applySeq :: PieceLike p =>
            p -> Theoretical.MoveType -> Theoretical.Sequence -> [Square]
applySeq p Theoretical.Takes seq =
  let pos = position p
      isCapturable sq = isEnemyAt pos sq ||
                        (isPawn p && isPassant pos sq)
  in take 1 $ filter isCapturable $ dropWhile (isEmpty pos) seq

applySeq p Theoretical.Moves seq =
  takeWhile (Position.isEmpty $ position p) seq
