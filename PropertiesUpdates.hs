module PropertiesUpdates where
import Prelude (undefined, succ)
import Data.Eq
import Data.Function
import Data.Map as Map hiding (mapMaybe)
import Data.Maybe
import Data.Bool
import Data.Monoid (mconcat)
import Text.Show
import Control.Monad
import Control.Monad.Reader
import qualified Data.Set as Set

import ListUtils
import MoveDescription
import Square
import Piece
import MoveType
import PositionReader
import qualified FullMove
import UpdateFunctions
import qualified Position
import Castling


-- The properties stack contains all updateFns that will update the position's meta info (e.g move count etc)
propertiesStack :: UpdateReader [UpdateFn]
propertiesStack = sequence [fullMove, halfMove, castlingRights, passantSquare]


fullMove :: UpdateReader UpdateFn
fullMove = do
    color <- whoseMoveR
    return $ case color of
        Black -> \p -> p { Position.fullMoveCount = succ (Position.fullMoveCount p) }
        White -> id


-- "This is the number of halfmoves since the last capture or pawn advance. 
--  This is used to determine if a draw can be claimed under the fifty-move rule."
halfMove :: UpdateReader UpdateFn
halfMove = do
    mv <- moveR
    let reset p = p { Position.halfMoveCount = 0 }
        inc p = p  { Position.halfMoveCount = succ (Position.halfMoveCount p) }
    return $ case mv of
        PawnMove _ _ -> reset
        OfficerMove _ (FullMove.Description _ _ Captures) -> reset
        _ -> inc


castlingRights :: UpdateReader UpdateFn
castlingRights = do
    mv <- moveR
    let rights = Map.fromList $
            [(square' "a1", [Castling White Queenside]),
             (square' "h1", [Castling White Kingside]),
             (square' "a8", [Castling Black Queenside]),
             (square' "h8", [Castling Black Kingside]),
             (square' "e1", [Castling White Kingside, Castling White Queenside]),
             (square' "e8", [Castling Black Kingside, Castling Black Queenside])]
        lost = Set.fromList $ mconcat $ mapMaybe (flip lookup rights) [FullMove.fullSource mv, destination mv]
    return $ \p -> p { Position.castlingRights = Position.castlingRights p `Set.difference` lost }


-- "En passant target square in algebraic notation. If there's no en passant target square, this is '-'. 
--  If a pawn has just made a two-square move, this is the position "behind" the pawn. 
--  This is recorded regardless of whether there is a pawn in position to make an en passant capture"
passantSquare :: UpdateReader UpdateFn
passantSquare = do
    mv <- moveR
    orig <- originalPositionR
    let psq = runReader (FullMove.passantSquare mv) orig
    return $ \p ->
        p { Position.passant = psq }


newTurn :: UpdateReader UpdateFn
newTurn = do
    color <- whoseMoveR
    return $ \p -> p { Position.turn = otherColor color }
