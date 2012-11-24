module Game where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import qualified Board 
import Piece
import Data.Maybe

data GameProperties = GameProperties {
    moveNumber :: Int,
    halfMoveNumber :: Int,
    whoseMove :: Color
    }
data Game = Game Board.Board GameProperties
type GameReader = Reader Game
type GameM e a = ErrorT e GameReader a


props (Game _ p) = p
board (Game g _) = g

pieceAt g = Board.pieceAt $ board g
squareIsEmpty g s = pieceAt g s == Nothing
