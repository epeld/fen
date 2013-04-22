module Move (
    Move, move,
    Move.position,
    ) where
import Prelude hiding (elem)
import Data.Maybe (maybe, fromJust)
import Control.Monad.Error ( throwError)

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
    elem,
    whichMoveType,
    )
import MoveType (
    MoveType(..)
    )

data Move = Move {
    movingPiece :: MovingPiece,
    destination :: Square,
    moveType :: MoveType,
    promotion :: Maybe Promotion
    }

move :: Position -> Square -> Square -> Maybe Promotion -> ErrorMonad Move
move p s d pr = do
    mp <- MovingPiece.movingPiece p s
    move' mp d pr

move' :: MovingPiece -> Square -> Maybe Promotion -> ErrorMonad Move
move' mp d pr = do
    mt <- identifyMoveType mp d
    let mv = Move mp d mt pr
    verifyPromotion (rank d) (color mp) (pieceType mp) pr
    return mv

identifyMoveType mp d = maybe (throwError NotInRange) return (whichMoveType mp d)

verifyPromotion 8 White Pawn Nothing = throwError LastRankPromote
verifyPromotion 1 Black Pawn Nothing = throwError LastRankPromote
verifyPromotion _ _ _ (Just _) = throwError NoPromotion
verifyPromotion _ _ _ _ = return ()

position = MovingPiece.position . Move.movingPiece
