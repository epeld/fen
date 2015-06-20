module PartialDescription where
import Prelude ()
import Data.Eq
import Data.Int
import Data.Bool
import Data.Char
import Data.Maybe
import Text.Show

import MoveDescription
import Square
import MoveType

data PartialSquare = Rank Int | File Char | Whole Square deriving Show

data Description = Description {
        destination :: Square, 
        moveType :: MoveType,
        source :: Maybe PartialSquare
    } deriving (Show)

instance MoveDescription Description where
    destination = PartialDescription.destination
    moveType = PartialDescription.moveType

    possibleSource desc sq = conforms (source desc)
        where conforms Nothing = True
              conforms (Just x) = conformsP x
              conformsP (Rank r) = rank sq == r
              conformsP (File f) = Just (file sq) == fileIndex f
              conformsP (Whole sq2) = sq == sq2

