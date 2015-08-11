{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module ParserUtils where
import Text.Parsec

import Parsable
import Square (Square, square', ranks, files)
import MoveType

import qualified PartialMove as Partial


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


rankPartial :: Parser Partial.PartialSquare
rankPartial = fmap Partial.File file


filePartial :: Parser Partial.PartialSquare
filePartial = fmap Partial.Rank rank


squarePartial :: Parser Partial.PartialSquare
squarePartial = fmap Partial.Whole square
