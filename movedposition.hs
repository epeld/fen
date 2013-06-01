module MovedPosition () where

import Position
import Move ( position, moveType, isPawnMove)
import Color ( Color(..),)
import MoveType (MoveType(..))

positionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

boardAfter mv = []

whoseTurnAfter = enemyColor. position

enPassantAfter mv = if isTwoStepPawnMove mv
    then Just (passantSquare mv) else Nothing

castlingRightsAfter mv = []

passantSquare mv = fromJust $ pawnDirection clr $ source mv
    where clr = whoseTurn. position $ mv
          pawnDirection White = up
          pawnDirection Black = down

fullMovesAfter mv = case whoseTurnAfter mv of
    White -> oldValue + 1
    Black -> oldValue
    where oldValue = fullMoves. position $ mv

-- Turns since the last pawn advance or capture
halfMovesAfter mv = if isPawnMove mv || moveType mv == Takes
    then 1 else oldValue + 1
    where oldValue = halfMoves. position $ mv
