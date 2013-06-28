module ChessMove (ChessMove(..)) where
import CastlingRight (Side)
import Move (Move)
import Position (Position)
import qualified Castles as Castling

data ChessMove = Standard Move | Castles Position Side 

castles p s = do
    Castling.verifyCanCastle p s
    return $ Castles p s
