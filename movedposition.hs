module MovedPosition () where
import Position
import Move ( position,)
import Color ( Color(..),)

positionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

boardAfter mv = []
whoseTurnAfter = enemyColor. position
enPassantAfter mv = Nothing
castlingRightsAfter mv = []

-- full moves increase every time black makes his move
fullMovesAfter mv = fullMovesAfter (whoseMove mv) mv
fullMovesAfter Black = (+1). fullMovesAfter' White
fullMovesAfter' White = fullMoves. position

-- Turns since the last pawn advance or capture
halfMovesAfter mv = halfMovesAfter' (pieceType mv) (moveType mv) mv
halfMovesAfter' Pawn _ mv = 1
halfMovesAfter' _ Takes mv = 1
halfMovesAfter' _ _ mv = (+1). halfMoves. position $ mv

replace s mp b = 
    let (first, second) = splitAt (fromEnum s) b
    in first ++ mp : drop 1 second
