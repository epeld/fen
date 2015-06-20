module PGN where
import Text.Parsec

import PartialDescription


data PGNMove = PGNMove PartialDescription Meta deriving Show

data Meta = Meta


