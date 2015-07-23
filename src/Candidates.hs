module Candidates where
import MoveType
import Piece
import Square
import PositionReader


candidates :: MoveType -> Piece -> Square -> PReader [Square]
candidates mt pc sq = catMaybe <$> forM (movementFn mt pc sq) (match pc) 


match :: Piece -> [Square] -> PReader (Maybe Square)
match pc [] = return Nothing
match pc (x : xs) = do
    mpc <- pieceAt x
    case mpc of
        Nothing -> 
            match pc xs

        Just pc' -> 
            if pc' == pc 
            then return (Just x)
            else return Nothing
