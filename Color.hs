module Color where
import Square

data Color = White | Black
           deriving (Show, Eq, Ord)

toggle :: Color -> Color
toggle White = Black
toggle Black = White

lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1

initialRank White = 2
initialRank Black = 7

forward White = up
forward Black = down
