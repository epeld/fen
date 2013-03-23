module Range (Range, range) where
import Internals (MovingPiece)
import Square (SquareSeries, above, below, leftOf, rightOf)
import Piece

import Control.Monad

type Range = [SquareSeries]

range mp = []

range' Pawn White s = []

data RangeType = Takes | Moves
pawnRange c s Moves = []
pawnMoves White 2 Moves = [above, above >=> above]
pawnMoves White _ Moves = [above]
pawnMoves Black 7 Moves = [below, below >=> below]
pawnMoves Black _ Moves = [below]
pawnMoves White _ Takes = [above >=> leftOf, above >=> rightOf]
pawnMoves Black _ Takes = [below >=> leftOf, below >=> rightOf]
