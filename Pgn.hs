module Pgn where
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>), (*>))
import Data.Char (digitToInt)
import qualified Chess
import qualified Fen

square :: Parser Chess.Square
square = Chess.Square <$> file <*> rank

rank :: Parser Int
rank = digitToInt <$> oneOf "12345678"

file :: Parser Char
file = oneOf "abcdefgh"


data Move = PieceMove {
                pieceType :: Chess.PieceType,
                source :: Maybe SourceIndicator,
                moveType :: MoveType,
                destination :: Square,
                promotion :: Maybe OfficerType } |
            Castles Chess.Side
            deriving (Show, Eq)

data MoveType = Moves | 
                Takes 
                deriving (Show, Eq)

data SourceIndicator = File Int | 
                       Rank Char | 
                       Square Chess.Square
                       deriving (Show, Eq)

pawnMove = shortPawnMove <|> longPawnMove


shortPawnMoves = movePawnTo <$> square <*> promotion

movePawnTo dest promo = PieceMove {
    pieceType = Chess.Pawn,
    source = Nothing,
    moveType = Moves,
    destination = dest,
    promotion = promo }

longPawnMove :: Parser Move
longPawnMove = (try longPawnMoves) <|> pawnTakes

longPawnMoves :: Parser Move
longPawnMoves = movePawnFromTo <$> square <*> square

movePawnFromTo src sq promo = PieceMove {
    pieceType = Chess.Pawn,
    source = Just src,
    moveType = Moves,
    destination = sq,
    promotion = promo }

pawnTakes :: Parser Move
pawnTakes = movePawnTakes <$> pawnSourceIndicator 
                          <*> char 'x' *> square
                          <*> promotion

promotion :: Parser (Maybe OfficerType)
promotion = option Nothing $ 
    char '=' >> fmap Fen.decode (oneOf "RNBQ")


movePawnTakes src sq promo = PieceMove {
                           pieceType = Chess.Pawn,
                           source = Just src,
                           moveType = Takes,
                           destination = sq,
                           promotion = promo }

                                

sourceIndicator :: Parser SourceIndicator
sourceIndicator = pawnSourceIndicator <|> rankIndicator

pawnSourceIndicator :: Parser SourceIndicator
pawnSourceIndicator = fileIndicator <|> squareIndicator

squareIndicator :: Parser SourceIndicator
squareIndicator = Square <$> square

fileIndicator :: Parser SourceIndicator
fileIndicator = File <$> file

rankIndicator :: Parser SourceIndicator
rankIndicator = Rank <$> rank
