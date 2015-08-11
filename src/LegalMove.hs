module LegalMove where
import Control.Lens
import Control.Applicative
import Control.Monad.Trans.Except
import Data.Maybe

import Move
import PartialMove
import FullMove
import MoveVerification as Verification
import Position
import LegalPosition
import PositionUpdates
import Candidates
import MoveType
import Piece


fullMove :: Position -> PartialMove -> Except [Verification.Error] FullMove
fullMove p mv = do
    mvs <- fullMoves p mv
    disambiguate mvs


-- Like `fullMove` but will not disambiguate
fullMoves :: Position -> PartialMove -> Except [Verification.Error] [FullMove]
fullMoves p mv = do
    verifyMoveType p mv
    return (legalCandidates p mv)


legalCandidates :: Position -> PartialMove -> [FullMove]
legalCandidates p mv = LegalMove.legalize p `mapMaybe` moveCandidates p mv


moveCandidates :: Position -> PartialMove -> [FullMove]
moveCandidates p mv = promote mv `mapMaybe` cands
    where
    cands = candidates p (mv ^. moveType) (mv ^. destination) pc
    pc = Piece (mv ^. Move.pieceType) (p ^. turn)


legalize :: Position -> FullMove -> Maybe FullMove
legalize p mv = do
    LegalPosition.legalize (after p mv)
    return mv
