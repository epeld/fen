{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module ParserUtils where
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Text.Parsec
import Data.Char

import ParserUtils
import MoveType
import Piece
import PartialMove
import qualified Square

import qualified PartialDescription as Partial

type Parser r = forall s. forall m. Stream s m Char => ParsecT s () m r

moveType :: Parser MoveType
moveType = Moves `option` captures
    where
    captures = do
        char 'x'
        return Captures


rankPartial :: Parser Partial.PartialSquare
rankPartial = Partial.File <$> file


filePartial :: Parser Partial.PartialSquare
filePartial = Partial.Rank <$> rank


squarePartial :: Parser Partial.PartialSquare
squarePartial = Partial.Whole <$> square


square :: Parser Square.Square
square = fromJust <$> liftM2 Square.square' file rank


file :: Parser Char
file = oneOf "abcdefgh"


rank :: Parser Int
rank = digitToInt <$> oneOf "12345678"
