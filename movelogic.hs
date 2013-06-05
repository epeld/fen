module MoveLogic (move, isPawnMove,
                  isPassantMove, isTwoStepPawnMove,) where
import Prelude hiding (elem)
import Data.Maybe (maybe, fromJust)
import Control.Monad (when, unless,)
import Control.Monad.Error (throwError)

import Move (Move(Move), pieceType, promotion, movingPiece, destination,
             position, square,)
import Square (Square, rank, twice, up)
import Piece (PieceType(..), OfficerType,)
import Color (Color(..))
import MoveType (MoveType(..))
import Position (Position, readSquare, Promotion, lastRank, isPassantSquare,
                 board, whoseTurn, board,)
import qualified ProjectedRange ( inferMoveType)
import ErrorMonad (ErrorMonad, 
                   Reason(NoPromotion, LastRankPromote, NotInRange),)
import MovingPiece (MovingPiece, color, isPawn)

move :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move mp d pr = do
    mt <- inferMoveType mp d
    let mv = Move mp mt d pr
    verifyPromotion mv
    return mv

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
requiresPromotion mv = isPawnMove mv && lastRankMove mv

isPawnMove = isPawn. movingPiece

lastRankMove :: Move -> Bool
lastRankMove mv = rank (destination mv) == Position.lastRank p
    where p = position mv

isTwoStepPawnMove :: Move -> Bool
isTwoStepPawnMove mv = twice up (square mv) == Just dest && isPawnMove mv
    where dest = destination mv

isPassantMove mv = isPassantSquare (position mv) (destination mv)
