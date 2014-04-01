module PgnParse where
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
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

pieceMove :: Parser Pgn.Move
pieceMove = (try longPieceMove) <|> shortPieceMove

move :: Parser Pgn.Move
move = try pawnMove <|> try pieceMove <|> castles

shortPieceMove :: Parser Pgn.Move
shortPieceMove = Pgn.PieceMove <$> fmap Chess.Officer officerType
                               <*> return Nothing
                               <*> moveType
                               <*> square
                               <*> return Nothing

longPieceMove :: Parser Pgn.Move
longPieceMove = Pgn.PieceMove <$> fmap Chess.Officer officerType
                              <*> fmap Just sourceIndicator
                              <*> moveType
                              <*> square
                              <*> return Nothing

officerType :: Parser Chess.OfficerType
officerType = fmap (fromJust. Fen.decodeChar) (oneOf "RNBQ")

moveType = option Pgn.Moves (string "x" >> return Pgn.Takes)

pawnMove :: Parser Pgn.Move
pawnMove = (try longPawnMove) <|> shortPawnMove

shortPawnMove :: Parser Pgn.Move
shortPawnMove = movePawnTo <$> square <*> maybePromotion

movePawnTo dest promo = Pgn.PieceMove {
    Pgn.pieceType = Chess.Pawn,
    Pgn.source = Nothing,
    Pgn.moveType = Pgn.Moves,
    Pgn.destination = dest,
    Pgn.promotion = promo }

longPawnMove :: Parser Pgn.Move
longPawnMove = (try longPawnMoves) <|> pawnTakes

longPawnMoves :: Parser Pgn.Move
longPawnMoves = movePawnFromTo <$> squareIndicator 
                               <*> square 
                               <*> maybePromotion

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
                          <*> maybePromotion

promotion :: Parser Chess.OfficerType
promotion = char '=' >> officerType

maybePromotion :: Parser (Maybe Chess.OfficerType)
maybePromotion = fmap Just promotion <|> return Nothing


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
