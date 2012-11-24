module Game where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import qualified Board 
import Piece
import Data.Maybe
import Square

data Side = Kingside | Queenside deriving (Show)
data CastlingRight = CastlingRight Side Color deriving (Show)

data GameProperties = GameProperties {
    whoseMove :: Color,
    castlingRights :: [CastlingRight],
    enPassantSquare :: Maybe Square,
    halfMoveNumber :: Int,
    moveNumber :: Int
    } deriving (Show)
data Game = Game Board.Board GameProperties deriving (Show)
type GameReader = Reader Game
type GameM e a = ErrorT e GameReader a


props (Game _ p) = p
board (Game g _) = g

pieceAt g = Board.pieceAt $ board g
squareIsEmpty g s = pieceAt g s == Nothing
