module Color ( Color(..), invert, lastRank, initialPawnRank) where

data Color = Black | White deriving (Show, Eq)

invert c = case c of
    White -> Black
    Black -> White

lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1

initialPawnRank White = 2
initialPawnRank Black = 7
