module Position where
import Color (Color)
import Move (Move(Move), Promotion)
import Piece (pieceType, color)

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

lookup :: Square -> Position -> Maybe Piece
lookup sq = Map.lookup sq. board

isEmpty :: Square -> Position -> Bool
isEmpty sq p = isNothing. lookup sq
