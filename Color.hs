module Color where
import Square
import FEN

data Color = White | Black
           deriving (Show, Eq, Ord)

toggle :: Color -> Color
toggle White = Black
toggle Black = White

lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1

initialRank :: Color -> Int
initialRank White = 2
initialRank Black = 7


instance FEN Color where
    encode White = "w"
    encode Black = "b"

    decode "w" = Just White
    decode "b" = Just Black
    decode _ = Nothing
