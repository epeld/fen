module MoveLogic (move, positionAfter,) where
import Prelude hiding (elem)
import Data.Maybe (maybe, fromJust)
import Control.Monad (when, unless,)
import Control.Monad.Error (throwError)

import Move (Move(Move), pieceType, promotion, 
             movingPiece, destination, position, square, 
             isPawnMove, isLastRankMove, moveType)
import MovedPosition (naivePositionAfter, )
import Threats (enemyKingIsSafe)
import Square (Square)
import Piece (PieceType(..), OfficerType,)
import Color (Color(..))
import MoveType (MoveType(..))
import Position (Position, readSquare, Promotion, board, whoseTurn, board,)
import qualified ProjectedRange ( inferMoveType)
import ErrorMonad (ErrorMonad, Reason(..))
import MovingPiece (MovingPiece)
import ChessMove

positionAfter mv = naivePositionAfter mv

move mp mt d pr = do
    mv <- move' mp d pr
    verifyMoveTypeMatch mt mv
    return mv

move' :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move' mp d pr = do
    mt <- inferMoveType mp d
    let mv = Move mp mt d pr
    verifyPromotion mv
    let p = naivePositionAfter (Standard mv)
    verifyKingIsSafe p
    return mv

verifyMoveTypeMatch mt mv = 
    unless (mt == moveType mv) $ throwError MoveTypeMismatch

verifyKingIsSafe p = unless (enemyKingIsSafe p) $ throwError KingCanBeCaptured

inferMoveType mp d = maybe (throwError NotInRange)
    return (ProjectedRange.inferMoveType mp d)

verifyPromotion mv = case pieceType mv of
    Pawn -> verifyPawnPromotion mv
    _ -> verifyOfficerPromotion mv

verifyOfficerPromotion mv = case promotion mv of
    Nothing -> return ()
    Just _ -> throwError NoPromotion

verifyPawnPromotion mv = case promotion mv of
    Nothing -> when (requiresPromotion mv) (throwError LastRankPromote)
    Just _ -> unless (requiresPromotion mv) (throwError NoPromotion)

requiresPromotion :: Move -> Bool
requiresPromotion mv = isPawnMove mv && isLastRankMove mv
