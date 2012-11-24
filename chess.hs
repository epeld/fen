module Chess where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Maybe
import Square
import Game
import Piece

color :: GameReader Color
color = asks $ whoseMove . props
color' = invert <$> color

pieceAt s = asks (!! s)

data MoveType = Moves | Takes deriving Eq


manyRelBelow s c = squareSeriesV s (down1 c) 
manyRelAbove s c = squareSeriesV s (up1 c) 

diagonals s = let dirs = [(a,b) | a <- [1,-1],b <- [1,-1]]
               in map (squareSeries s) dirs

straights s = map (squareSeries s) [(1,0), (-1,0), (0,-1), (0,1)]

knights :: Square -> [Square]
knights s = let offs = liftM2 (,) [2,-2] [1,-1] ++ liftM2 (,) [1,-1] [2,-2]
                extractValidOnes = map fromJust . filter isJust
             in extractValidOnes $ offset s <$> offs

squares t m s c =
    case t of
        Pawn -> if m == Takes 
            then let d = down1 c
                  in return $ offsets s [(1,d), (-1,d)]
            else return $ take 2 $ manyRelBelow s c
        _ -> pieceSquares t s

pieceSquares t s =
    case t of
        Bishop -> diagonals s
        Queen  -> pieceSquares Bishop s ++ pieceSquares Rook s
        King   -> take 1 <$> pieceSquares Queen s
        Rook   -> straights s
        Knight -> return <$> knights s


--candidates t m s =
--    let hasCand s =
--    filter hasCand <$> (squares t m s)


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
