module Theoretical where
import Stepping (forward, left, right, compose)
import PieceLike (color, square)
import Square (Square)

type Sequence = [Square]
type Range = [Sequence]

data MoveType = Moves | Takes deriving (Show, Eq)

range :: PieceLike p => p -> MoveType -> Range
range p mt =
  case pieceType p of
    Pawn -> pawnRange mt (color p) (square p)
    Officer off -> officerRange off (square p)

pawnRange :: PieceLike p => MoveType -> Color -> Square -> Range
pawnRange Takes c sq =
  catMaybes [compose [forward c, left] sq,
             compose [forward c, right] sq]

pawnRange Moves c sq =
  let numSquares = if initialRank c == rank sq then 2 else 1
  in take numSquares $ run (forward c) sq

officerRange :: OfficerType -> Square -> Range
officerRange officer sq = steppers officer `forEach` stepAs officer
  where stepAs King = take 1. step sq
        stepAs Knight = stepAs King
        stepAs off = step sq

step :: Square -> Stepper -> [Square]
step = flip run

forEach :: [a] -> (a -> b) -> [b]
forEach = flip map
