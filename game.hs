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
data Game = Game {
    board :: Board.Board,
    properties :: GameProperties
    } deriving (Show)
type GameReader = Reader Game
type GameM e a = ErrorT e GameReader a

replace i e l = 
    let start = take i l
        end = drop (i+1) l
     in start ++ e : end

move s d b = replace s Nothing . replace d (b !! s)

updateBoard s d g = Game.move s d (board g)

isPassantSquare d (Game _ p) =
    Just d == enPassantSquare p
