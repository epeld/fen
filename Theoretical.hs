module Theoretical where
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>))

import qualified Stepping
import qualified PieceLike
import qualified Square
import qualified Piece
import qualified Color

type Sequence = [Square.Square]
type Range = [Sequence]

data MoveType = Moves | Takes deriving (Show, Eq)

range :: PieceLike.PieceLike p =>
         p -> MoveType -> Range
range p mt =
  case PieceLike.pieceType p of
    Piece.Pawn -> pawnRange mt (PieceLike.color p) (PieceLike.square p)
    Piece.Officer off -> officerRange off (PieceLike.square p)

-- TODO Implementation hard to understand. Fix.
pawnRange :: MoveType -> Color.Color -> Square.Square -> Range
pawnRange Takes c sq =
  let ahead = Stepping.forward c
      directions = [[ahead, Stepping.left], [ahead, Stepping.right]]
  in map (:[]) $ catMaybes $ Stepping.compose <$> directions <*> [sq]

pawnRange Moves c sq =
  let numSquares = if Color.initialRank c == Square.rank sq then 2 else 1
      ahead = Stepping.forward c
  in [take numSquares $ Stepping.run ahead sq]

officerRange :: Piece.OfficerType -> Square.Square -> Range
officerRange officer sq = Stepping.steppers officer `forEach` stepAs officer
  where stepAs Piece.King = take 1. step sq
        stepAs Piece.Knight = stepAs Piece.King
        stepAs off = step sq

step :: Square.Square -> Stepping.Stepper -> [Square.Square]
step = flip Stepping.run

forEach :: [a] -> (a -> b) -> [b]
forEach = flip map
