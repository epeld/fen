module Range (Range) where
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
    up, down, left, right,
    upLeft, upRight, downLeft, downRight,
    twice,
    rank
    )
import Piece (
    OfficerType(..),
    Piece,
    PieceType(..),
    Color(..)
    )

import Data.Maybe (isNothing, fromJust)
import Control.Monad (
    (>=>),
    liftM
    )
import Control.Applicative ((<*>), (<$>))

data Range = Range {
    pieceType :: PieceType,
    square :: Square,
    squares :: [SquareSeries]
} deriving (Show)

{-
range :: MovingPiece -> Range
range mp = rangeOfPiece (p `readSquare` s) s
    where p = position mp
          s = square mp
          rangeOfPiece Nothing = error "Invalid MovingPiece in range"
          rangeOfPiece (Just pc) = range' p (pieceType pc) (color pc)

range' :: LegalPosition -> PieceType -> Color -> Square -> Range
range' p Pawn c s = Range Pawn
-}

officerRange :: OfficerType -> Square -> Range
officerRange ot s = Range (Officer ot) s $ officerSquares ot s

isJustSquare = not. isNothing

pawnRange :: Color -> Square -> MoveType -> Range
pawnRange c s mt = Range Pawn s $ pawnSquares c s mt

pawnSquares c s Moves = return. squareSequence $
    pawnSquares' c s Moves

pawnSquares c s Takes = return <$> squareSet $
    pawnSquares' c s Takes 

pawnSquares' :: Color -> Square -> MoveType -> [Maybe Square]
pawnSquares' c s mt = pawnMoves c (rank s) mt <*> [s]

officerSquares :: OfficerType -> Square -> [SquareSeries]
officerSquares ot s = squareSequence <$> officerSquaresM ot s

type Reducer = (Maybe Square -> Bool) -> [Maybe Square] -> [Maybe Square]

toSeries :: Reducer -> [Maybe Square] -> SquareSeries
toSeries r sqs = fromJust <$> r isJustSquare sqs

squareSet :: [Maybe Square] -> SquareSeries
squareSet = toSeries takeWhile

squareSequence :: [Maybe Square] -> SquareSeries
squareSequence = toSeries filter

iterateMove :: (Square -> Maybe Square) -> Square -> [Maybe Square]
iterateMove m = iterate (>>= m) . return

pawnMoves :: Color -> Int -> MoveType -> [Square -> Maybe Square]
pawnMoves White 2 Moves = [up, twice up]
pawnMoves White _ Moves = [up]
pawnMoves Black 7 Moves = [down, twice down]
pawnMoves Black _ Moves = [down]
pawnMoves White _ Takes = [upLeft, upRight]
pawnMoves Black _ Takes = [downLeft, downRight]

onceTwice m m' = m' >=> twice m
knightMoves = onceTwice <$> [up, down] <*> [left, right]

officerDirections Bishop = [upLeft, upRight, downLeft, downRight]
officerDirections Rook = [up, down, left, right]
officerDirections Queen = concat $
    map officerDirections [Rook, Bishop]

officerSquaresM :: OfficerType -> Square -> [[Maybe Square]]
officerSquaresM King s = take 1 <$> officerSquaresM Queen s
officerSquaresM Knight s = liftM return (knightMoves <*> [s])
officerSquaresM ot s = flip iterateMove s <$> officerDirections ot
    
