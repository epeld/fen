module Chess where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Cont
import Square
import Game
import Piece

color :: GameReader Color
color = asks $ whoseMove . props
color' = invert <$> color


leftOf :: Square -> GameReader (Maybe Square)
leftOf sq =  hoffset sq . left1 <$> color

pieceAt s = asks $ (!! s)


-- Find all candidates able to reach s
{--
findCandidates :: Square -> GameM String [Square]
findCandidates s = do
    p <- pieceAt s
    c <- color
    case p of
        Just (Piece _ c2) -> if c /= c2
            then return $ takers s
            else fail "Can't take same-colored piece"
        Nothing -> movers s

takers s = return []
movers s = return []
--}
