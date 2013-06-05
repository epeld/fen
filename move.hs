module ChessMove (ChessMove) where
import StandardMove (StandardMove)
import CastlingRight (Side)

data ChessMove = Either CastlingMove StandardMove

data CastlingMove = Castles Side deriving (Show, Eq)
