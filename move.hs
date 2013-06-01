module Move ( Move, move, Move.position, isPawnMove, moveType,
              Move.movingPiece, destination, promotion) where

import Prelude hiding (elem)
import Data.Maybe (maybe, fromJust)
import Control.Monad (when)
import Control.Monad.Error ( throwError)

import Square ( Square, rank, twice, up)
import Piece ( PieceType(..), OfficerType,)
import Color ( Color(..))
import MoveType ( MoveType(..))
import Position ( Position, readSquare, Promotion, lastRank, isPassantSquare)
import qualified ProjectedRange ( inferMoveType)
import ErrorMonad ( ErrorMonad, 
                    Reason(NoPromotion, LastRankPromote, NotInRange),)
import MovingPiece ( MovingPiece, position, square, color, 
                     pieceType, movingPiece, isPawn)

data Move = Move { movingPiece :: MovingPiece, moveType :: MoveType,
                   destination :: Square,  promotion :: Maybe Promotion }

move :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move mp d pr = do
    mt <- inferMoveType mp d
    let mv = Move mp mt d pr
    verifyPromotion mv
    return mv

inferMoveType mp d = maybe (throwError NotInRange)
    return (ProjectedRange.inferMoveType mp d)

verifyPromotion mv = case Move.pieceType mv of
    Pawn -> verifyPawnPromotion mv
    _ -> verifyOfficerPromotion mv

verifyOfficerPromotion mv = case promotion mv of
    Nothing -> return ()
    Just _ -> throwError NoPromotion

verifyPawnPromotion mv = case promotion mv of
    Nothing -> when (requiresPromotion mv) (throwError LastRankPromote)
    Just _ -> when (not. requiresPromotion $ mv) (throwError NoPromotion)

requiresPromotion :: Move -> Bool
requiresPromotion mv = isPawnMove mv && lastRankMove mv

lastRankMove :: Move -> Bool
lastRankMove mv = rank (destination mv) == Position.lastRank p
    where p = Move.position mv

isTwoStepPawnMove :: Move -> Bool
isTwoStepPawnMove mv = twice up (Move.square mv) == Just dest && isPawnMove mv
    where dest = destination mv

--initialPawnRankMove mv = rank (square mv) == initialPawnRank p
--    where p = Move.position mv

isPassantMove mv = isPassantSquare (Move.position mv) (destination mv)

square = MovingPiece.square. Move.movingPiece
position = MovingPiece.position. Move.movingPiece
pieceType = MovingPiece.pieceType. Move.movingPiece
isPawnMove = isPawn. Move.movingPiece
