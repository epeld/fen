module Range (Range, range) where
import Internals (
    readSquare,
    LegalPosition,
    MoveType
        (Takes, Moves)
    )
import MovingPiece (MovingPiece, position, square)
import Square (
    Square,
    SquareSeries,
    above,
    below,
    leftOf,
    rightOf,
    rank
    )
import Piece

import Data.Maybe (isNothing, fromJust)
import Control.Monad ((>=>))
import Control.Applicative ((<*>), (<$>))

type Range = [SquareSeries]

range :: MovingPiece -> Range
range mp = rangeOfPiece (p `readSquare` s) s
    where p = position mp
          s = square mp
          rangeOfPiece Nothing = error "Invalid MovingPiece in range"
          rangeOfPiece (Just pc) = range' p (pieceType pc) (color pc)

range' :: LegalPosition -> PieceType -> Color -> Square -> Range
range' p Pawn c s = []

isJustSquare = not. isNothing

pawnRange :: Color -> Square -> MoveType -> Range
pawnRange c s Moves = return $ squareSequence $
    pawnRange' c s Moves

pawnRange c s takes = return <$> squareSet $
    pawnRange' c s Takes 

pawnRange' :: Color -> Square -> MoveType -> [Maybe Square]
pawnRange' c s mt = pawnMoves c (rank s) mt <*> [s]

type Reducer = (Maybe Square -> Bool) -> [Maybe Square] -> [Maybe Square]

toSeries :: Reducer -> [Maybe Square] -> SquareSeries
toSeries r sqs = fromJust <$> r isJustSquare sqs

squareSet :: [Maybe Square] -> SquareSeries
squareSet = toSeries takeWhile

squareSequence :: [Maybe Square] -> SquareSeries
squareSequence = toSeries filter

pawnMoves White 2 Moves = [above, above >=> above]
pawnMoves White _ Moves = [above]
pawnMoves Black 7 Moves = [below, below >=> below]
pawnMoves Black _ Moves = [below]
pawnMoves White _ Takes = [above >=> leftOf, above >=> rightOf]
pawnMoves Black _ Takes = [below >=> leftOf, below >=> rightOf]
