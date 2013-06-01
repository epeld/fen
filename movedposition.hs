module MovedPosition () where
import Control.Monad.State (runState)
import Control.Monad (when)
import Data.Maybe (fromJust)

import Position (Position(Position), enemyColor, whoseTurn,
                 fullMoves, halfMoves,)
import Board (move, remove,)
import Move (position, moveType, isPawnMove, square, destination,
             isPassantMove, isTwoStepPawnMove, whose, board,)
import Color (Color(..), invert)
import MoveType (MoveType(..))
import Square (up, down)
import PawnRange (pawnDirection,)

positionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

boardAfter mv = snd $ runState (makeMove mv) (board mv)

makeMove mv = do
    move (square mv) (destination mv)
    when (isPassantMove mv) $ do
        removePassantPawn mv
        return ()

removePassantPawn mv = remove . passantSquare $ mv

passantSquare mv = fromJust $ backward $ destination mv
    where backward = pawnDirection $ invert $ whose mv

whoseTurnAfter = enemyColor. position

enPassantAfter mv = if isTwoStepPawnMove mv
    then Just $ passantSquare mv else Nothing

castlingRightsAfter mv = []

fullMovesAfter mv = case whoseTurnAfter mv of
    White -> oldValue + 1
    Black -> oldValue
    where oldValue = fullMoves. position $ mv

-- Turns since the last pawn advance or capture
halfMovesAfter mv = if isPawnMove mv || moveType mv == Takes
    then 1 else oldValue + 1
    where oldValue = halfMoves. position $ mv
