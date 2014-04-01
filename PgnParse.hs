module PgnParse where
import Data.Char (digitToInt)
import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec
import qualified Pgn
import qualified Chess
import qualified Fen

square :: Parser Chess.Square
square = Chess.Square <$> file <*> rank

rank :: Parser Int
rank = digitToInt <$> oneOf "12345678"

file :: Parser Char
file = oneOf "abcdefgh"

castles = Pgn.Castles <$> (try queenside <|> kingside)
queenside = string "O-O-O" >> return Chess.Queenside
kingside = string "O-O" >> return Chess.Kingside

pawnMove :: Parser Pgn.Move
pawnMove = shortPawnMove <|> longPawnMove

shortPawnMove :: Parser Pgn.Move
shortPawnMove = movePawnTo <$> square <*> promotion

movePawnTo dest promo = Pgn.PieceMove {
    Pgn.pieceType = Chess.Pawn,
    Pgn.source = Nothing,
    Pgn.moveType = Pgn.Moves,
    Pgn.destination = dest,
    Pgn.promotion = promo }

longPawnMove :: Parser Pgn.Move
longPawnMove = (try longPawnMoves) <|> pawnTakes

longPawnMoves :: Parser Pgn.Move
longPawnMoves = movePawnFromTo <$> squareIndicator <*> square <*> promotion

movePawnFromTo :: Pgn.SourceIndicator -> Chess.Square -> 
                  Maybe Chess.OfficerType -> Pgn.Move
movePawnFromTo src sq promo = Pgn.PieceMove {
    Pgn.pieceType = Chess.Pawn,
    Pgn.source = Just src,
    Pgn.moveType = Pgn.Moves,
    Pgn.destination = sq,
    Pgn.promotion = promo }

pawnTakes :: Parser Pgn.Move
pawnTakes = movePawnTakes <$> pawnSourceIndicator 
                          <*> (char 'x' >> square)
                          <*> promotion

promotion :: Parser (Maybe Chess.OfficerType)
promotion = option Nothing $ 
    char '=' >> fmap Fen.decodeChar (oneOf "RNBQ")


movePawnTakes src sq promo = Pgn.PieceMove {
    Pgn.pieceType = Chess.Pawn,
    Pgn.source = Just src,
    Pgn.moveType = Pgn.Takes,
    Pgn.destination = sq,
    Pgn.promotion = promo }

                                

sourceIndicator :: Parser Pgn.SourceIndicator
sourceIndicator = pawnSourceIndicator <|> rankIndicator

pawnSourceIndicator :: Parser Pgn.SourceIndicator
pawnSourceIndicator = fileIndicator <|> squareIndicator

squareIndicator :: Parser Pgn.SourceIndicator
squareIndicator = Pgn.Square <$> square

fileIndicator :: Parser Pgn.SourceIndicator
fileIndicator = Pgn.File <$> file

rankIndicator :: Parser Pgn.SourceIndicator
rankIndicator = Pgn.Rank <$> rank
