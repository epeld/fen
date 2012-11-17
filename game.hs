module Game where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Board
import Piece

data GameProperties = GameProperties {
    moveNumber :: Int,
    halfMoveNumber :: Int,
    whoseMove :: Color
    }
data Game = Game Board GameProperties
type GameReader = Reader Game
type GameM e a = ErrorT e GameReader a


props (Game _ p) = p
