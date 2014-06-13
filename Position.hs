module Position where
import Color

type Board = Map Square Piece

data Position = Position { board :: Board,
                           passant :: Maybe Square,
                           halfMoveNr :: Int,
                           fullMoveNr :: Int,
                           turn :: Color,
                           castling :: Set CastlingRight }
                           deriving (Show, Eq)

lastRank :: Position -> Int
lastRank = Color.lastRank. turn

nextTurn :: Position -> Position
nextTurn p = p{turn = enemyColor p }

enemyColor :: Position -> Color
enemyColor = Color.toggle. turn

-- Calculate the square that was captured en passant by a move (if any)
passantCapture :: Move -> Position -> Maybe Square
passantCapture mv p =
    let src = source mv
        dest = destination mv
    captureSquare <- back dest
    plausible <- everyM [pawnAt dest,
                         maybeNot pawnAt captureSquare,
                         maybeNot enemyAt captureSquare,
                         isPassantSquare dest,
                         isForward 1 src dest]
    return $
        if adjacentFiles src dest && plausible
        then captureSquare
        else Nothing
