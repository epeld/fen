module Game where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Applicative
import qualified Board 
import Piece
import Data.Maybe
import Square

data Side = Kingside | Queenside deriving (Show, Eq)
data CastlingRight = CastlingRight Side Color deriving (Show, Eq)

sides = [Queenside, Kingside]
colorsRight = flip CastlingRight
whitesRight = colorsRight White
blacksRight = colorsRight Black
blacksRights = blacksRight <$> sides
whitesRights = whitesRight <$> sides

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

replace :: Int -> a -> [a] -> [a]
replace i e l = 
    let start = take i l
        end = drop (i+1) l
     in start ++ e : end

replace' s mp b = replace (fromEnum s) mp b

move :: Int -> Int -> [Maybe Piece] -> [Maybe Piece]
move s d b = replace s Nothing . replace d (b !! s) $ b
move' s d b =
    let s' = fromEnum s
        d' = fromEnum d
     in
        move s' d' b

updateBoard s d g = Game.move s d (board g)

isPassantSquare d (Game _ p) =
    Just d == enPassantSquare p
