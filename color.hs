module Color (Color(..), invert, lastRank, initialPawnRank, colorToChar,
              firstRank) where

data Color = Black | White deriving (Show, Eq, Enum)

invert c = case c of
    White -> Black
    Black -> White

colorToChar White = 'w'
colorToChar Black = 'b'

lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1

firstRank = lastRank . invert

initialPawnRank White = 2
initialPawnRank Black = 7
