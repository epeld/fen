module Color (Color(..), invert, lastRank, initialPawnRank,
              firstRank) where

data Color = Black | White deriving (Show, Eq, Enum)

invert c = case c of
    White -> Black
    Black -> White

lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1

firstRank = lastRank . invert

initialPawnRank White = 2
initialPawnRank Black = 7
