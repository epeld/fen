module PropertiesUpdates where
import Prelude (undefined, succ)
import Data.Eq
import Data.Function
import Data.Map
import Data.Maybe
import Data.Bool
import Text.Show
import Control.Monad
import Control.Monad.Reader

import ListUtils
import MoveDescription
import Square
import Piece
import MoveType
import PositionReader
import qualified FullDescription as FullMove
import UpdateFunctions
import qualified Position


-- The properties stack contains all updateFns that will update the position's meta info (e.g move count etc)
propertiesStack :: UpdateReader [UpdateFn]
propertiesStack = sequence [fullMove, halfMove, castlingRights, passantSquare]


fullMove :: UpdateReader UpdateFn
fullMove = do
    color <- asks (Position.turn. originalPosition)
    return $ case color of
        Black -> \p -> p { Position.fullMoveCount = succ (Position.fullMoveCount p) }
        White -> id


-- "This is the number of halfmoves since the last capture or pawn advance. 
--  This is used to determine if a draw can be claimed under the fifty-move rule."
halfMove :: UpdateReader UpdateFn
halfMove = do
    mv <- asks move
    let reset p = p { Position.halfMoveCount = 0 }
        inc p = p  { Position.halfMoveCount = succ (Position.halfMoveCount p) }
    return $ case mv of
        PawnMove _ _ -> reset
        OfficerMove _ (FullMove.Description _ _ Captures) -> reset
        _ -> inc


castlingRights :: UpdateReader UpdateFn
castlingRights = do
    mv <- asks move
    return id -- TODO


-- "En passant target square in algebraic notation. If there's no en passant target square, this is '-'. 
--  If a pawn has just made a two-square move, this is the position "behind" the pawn. 
--  This is recorded regardless of whether there is a pawn in position to make an en passant capture"
passantSquare :: UpdateReader UpdateFn
passantSquare = do
    mv <- asks move
    orig <- asks originalPosition 
    let psq = runReader (FullMove.passantSquare mv) orig
    return $ \p ->
        p { Position.passant = psq }


newTurn :: UpdateReader UpdateFn
newTurn = do
    color <- asks (Position.turn. originalPosition)
    return $ \p -> p { Position.turn = otherColor color }
