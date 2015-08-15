{-#LANGUAGE NoMonomorphismRestriction #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
module Parsable where
import Control.Monad
import Text.Parsec

import Castling
import MoveType
import Piece


type Parser r = forall s. forall m. Stream s m Char => ParsecT s () m r


class Parsable p where
    rep :: p -> String


-- Tries the parsers in order until one succeeds
choose :: Parsable p => [p] -> Parser p
choose ps = tryChoices (fmap Parsable.parse ps)


tryChoices ps = choice (fmap try ps) -- Add 'try' so that each new parser starts from the beginning!


parse :: Parsable p => p -> Parser p
parse p = do
    string (rep p)
    return p


instance Parsable Side where
    rep Kingside = "O-O"
    rep Queenside = "O-O-O"


instance Parsable OfficerType where
    rep Rook = "R"
    rep Bishop = "B"
    rep Knight = "N"
    rep Queen = "Q"
    rep King = "K"


instance Parsable MoveType where
    rep Moves = ""
    rep Captures = "x"


instance Parsable Char where
    rep c = [c]


instance Parsable Int where
    rep i = show i
