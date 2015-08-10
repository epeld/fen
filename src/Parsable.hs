module Parsable where
import Control.Monad
import Text.Parsec

import ParserUtils
import Castling


class Parsable p where
    rep :: p -> String


anyOf :: Parsable p => [p] -> Parser p
anyOf = choice (fmap parse p)


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
