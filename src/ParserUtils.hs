{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module ParserUtils where
import Text.Parsec

import Square
import MoveType

import PartialDescription as Partial

type Parser r = forall s. forall m. Stream s m Char => ParsecT s () m r


square :: Parser Square
square = do
    f <- file
    r <- rank
    case square' f r of
        Just sq -> return sq
        Nothing -> error "Internal error: square parsing" -- Should never happen


file :: Parser Char
file = choose files


rank :: Parser Int
rank = choose ranks


moveType :: Parser MoveType
moveType = choose [Captures, Moves]


rankPartial :: Parser PartialSquare
rankPartial = fmap Partial.File file


filePartial :: Parser PartialSquare
filePartial = fmap Partial.Rank rank


squarePartial :: Parser PartialSquare
squarePartial = Partial.Whole <$> square
