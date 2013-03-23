module Range (Range, range) where
import Internals (readSquare, LegalPosition)
import MovingPiece (MovingPiece, position, square)
import Square (Square, SquareSeries, above, below, leftOf, rightOf)
import Piece

import Control.Monad

type Range = [SquareSeries]

range :: MovingPiece -> Range
range mp = rangeOfPiece (p `readSquare` s) s
    where p = position mp
          s = square mp
          rangeOfPiece Nothing = error "Invalid MovingPiece in range"
          rangeOfPiece (Just pc) = range' p (pieceType pc) (color pc)

range' :: LegalPosition -> PieceType -> Color -> Square -> Range
range' p Pawn White s = []

data RangeType = Takes | Moves
pawnRange c s Moves = []
pawnMoves White 2 Moves = [above, above >=> above]
pawnMoves White _ Moves = [above]
pawnMoves Black 7 Moves = [below, below >=> below]
pawnMoves Black _ Moves = [below]
pawnMoves White _ Takes = [above >=> leftOf, above >=> rightOf]
pawnMoves Black _ Takes = [below >=> leftOf, below >=> rightOf]
