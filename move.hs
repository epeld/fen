module Move (Move, move) where

import Square (
    Square, rank
    )
import Piece (
    PieceType(..), 
    OfficerType,
    )
import Color (
    Color(..)
    )
import ErrorMonad (
    ErrorMonad,
    Reason(NoPromotion, LastRankPromote, NotInRange),
    )
import Position (
    Position,
    readSquare,
    Promotion,
    )
import MovingPiece (
    MovingPiece,
    position, square,
    color, pieceType,
    movingPiece,
    )
import ProjectedRange (
    projectedRange,
    elem
    )
import MoveType (
    MoveType(..)
    )

import Prelude hiding (elem)
import Data.Maybe ( fromJust)
import Control.Monad.Error ( throwError)

data Move = Move {
    movingPiece :: MovingPiece,
    destination :: Square,
    promotion :: Maybe Promotion
    }
data ClassifiedMove = Standard Move | Capturing Move

move :: Position -> Square -> Square -> Maybe Promotion -> ErrorMonad ClassifiedMove
move p s d pr = do
    mp <- MovingPiece.movingPiece p s
    move' mp d pr

move' :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad ClassifiedMove
move' mp d pr = do
    mv <- classify $ Move mp d pr
    verifyPromotion (rank d) (color mp) (pieceType mp) pr
    return mv

classify :: Move -> ErrorMonad ClassifiedMove
classify m = case (moveable m, takeable m) of
    (True, True)  -> error "classify confused :("
    (True, False) -> return $ Standard m
    (False, False) -> throwError NotInRange
    (False, True) -> return $ Capturing m

inRange mt m = destination m `elem` projectedRange (Move.movingPiece m) mt
moveable = inRange Moves
takeable = inRange Takes

verifyPromotion 8 White Pawn Nothing = throwError LastRankPromote
verifyPromotion 1 Black Pawn Nothing = throwError LastRankPromote
verifyPromotion _ _ _ (Just _) = throwError NoPromotion
verifyPromotion _ _ _ _ = return ()
