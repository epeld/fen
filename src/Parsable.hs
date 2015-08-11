module Parsable where
import Control.Monad
import Text.Parsec

import ParserUtils
import Castling


class Parsable p where
    rep :: p -> String


-- Tries the parsers in order until one succeeds
choose :: Parsable p => [p] -> Parser p
choose = choice (fmap parse p)


parse :: Parsable p => Parser p
parse = do
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
