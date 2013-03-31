module MovedPosition (
    ) where
import Position
import Color (
    Color(..)
    )

positionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

boardAfter mv = []
whoseTurnAfter mv = White
enPassantAfter mv = Nothing
castlingRightsAfter mv = []
fullMovesAfter mv = 1
halfMovesAfter mv = 1
