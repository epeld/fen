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

data MoveType = Moves | Takes deriving (Eq, Show)


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
        Officer t' -> officerSquares t' s

officerSquares t s =
    case t of
        Bishop -> diagonals s
        Queen  -> officerSquares Bishop s ++ officerSquares Rook s
        King   -> take 1 <$> officerSquares Queen s
        Rook   -> straights s
        Knight -> return <$> knights s

firstNonEmpties :: Game -> [[Square]] -> [Square]
firstNonEmpties g s =
    let firstNonEmpty = take 1 . dropWhile (squareIsEmpty g)
     in concat $ map firstNonEmpty s

matchPiece mp p = mp == Just p

candidates t m s = do
    c <- color
    g <- ask
    let hasCand s = pieceAt g s `matchPiece` Piece t c
     in return $ filter hasCand $ firstNonEmpties g (squares t m s c)
