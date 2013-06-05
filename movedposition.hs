module MovedPosition (naivePositionAfter, kingIsSafe,) where
import Control.Monad.State (runState)
import Control.Monad (when)
import Control.Applicative ((<$>))
import Data.Either (rights, lefts)
import Data.Maybe (fromJust)

import Board (move, remove,)
import StandardMove (position, moveType, square, destination, whose, board,
                     isPawnMove, isTwoStepPawnMove, isPassantMove)
import Position (Position(Position), enemyColor, whoseTurn,
                 fullMoves, halfMoves, friendlySquares, enemy,)
import Piece (PieceType(Officer), OfficerType(King))
import Color (Color(..), invert)
import MoveType (MoveType(..))
import Square (up, down)
import PawnRange (pawnDirection,)
import MovingPiece (movingPiece,)
import ProjectedRange (threatens,)

kingIsSafe p = not. any (threatens enemyKingSq) $ friendlies p
    where enemyKingSq = enemy (Officer King) p

friendlies p = case lefts mps of
    [] -> rights mps
    x  -> error $ show $ head x
    where mps = movingPiece p <$> friendlySquares p

naivePositionAfter mv = Position
    (boardAfter mv)
    (whoseTurnAfter mv)
    (enPassantAfter mv)
    (castlingRightsAfter mv)
    (fullMovesAfter mv)
    (halfMovesAfter mv)

boardAfter mv = snd $ runState (makeMove mv) (board mv)

makeMove mv = do
    Board.move (square mv) (destination mv)
    when (isPassantMove mv) $ do
        removePassantPawn mv
        return ()

removePassantPawn mv = remove . passantSquare $ mv

passantSquare mv = fromJust $ backward $ destination mv
    where backward = pawnDirection $ invert $ whose mv

whoseTurnAfter = enemyColor. position

enPassantAfter mv = if isTwoStepPawnMove mv
    then Just $ passantSquare mv else Nothing

castlingRightsAfter mv = []

fullMovesAfter mv = case whoseTurnAfter mv of
    White -> oldValue + 1
    Black -> oldValue
    where oldValue = fullMoves. position $ mv

-- Turns since the last pawn advance or capture
halfMovesAfter mv = if isPawnMove mv || moveType mv == Takes
    then 1 else oldValue + 1
    where oldValue = halfMoves. position $ mv
