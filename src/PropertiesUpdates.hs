module PropertiesUpdates where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable
import Data.Monoid
import Control.Monad.Reader
import Control.Applicative
import Control.Lens

import Castling
import ListUtils
import Square
import Piece
import MoveType
import Move
import FullMove
import PawnMovement
import PositionProperties as Properties

type Update = Properties -> Properties 
type UpdateReader = Reader (FullMove, Properties)


updates :: [UpdateReader Update]
updates = [fullMove, halfMove, PropertiesUpdates.castlingRights, passantSquare]


fullMove :: UpdateReader Update
fullMove = do
    (_, p) <- ask
    return $ case p ^. Properties.turn of
        Black -> fullMoveCount +~ 1
        White -> id


-- "This is the number of halfmoves since the last capture or pawn advance. 
--  This is used to determine if a draw can be claimed under the fifty-move rule."
halfMove :: UpdateReader Update
halfMove = do
    (Full mv, _) <- ask
    return $ case mv of
        PawnMove _ _ -> halfMoveCount .~ 0

        OfficerMove _ _ ->
            if mv ^. moveType == Captures
            then halfMoveCount .~ 0
            else halfMoveCount +~ 1


castlingRights :: UpdateReader Update
castlingRights = do
    (mv, _) <- ask
    return $ Properties.castlingRights %~ (`Set.difference` lostCastlingRights mv)



-- "En passant target square in algebraic notation. If there's no en passant target square, this is '-'. 
--  If a pawn has just made a two-square move, this is the position "behind" the pawn. 
--  This is recorded regardless of whether there is a pawn in position to make an en passant capture"
passantSquare :: UpdateReader Update
passantSquare = do
    (Full mv, p) <- ask
    return $
        case mv of
            PawnMove _ _ -> passant .~ behind (mv ^. destination) (p ^. turn)
            _ -> id


newTurn :: UpdateReader Update
newTurn = return $ turn %~ otherColor


lostCastlingRights :: FullMove -> Set.Set CastlingRight
lostCastlingRights (Full mv) = 
    case foldMap (`Map.lookup` castlingRightsMap) sqs of
        Nothing -> Set.empty
        Just s -> s
    where 
    sqs = [mv ^. source, mv ^. destination]


-- Map of which castling rights you lose if your move 'touches' a specific square (source or destination)
castlingRightsMap :: Map.Map Square (Set.Set CastlingRight)
castlingRightsMap = Map.map Set.fromList $ Map.mapKeys Square.unsafe $ Map.fromList pairs
    where
    pairs = [
        ("a1", [Castling White Queenside]),
        ("h1", [Castling White Kingside]),
        ("a8", [Castling Black Queenside]),
        ("h8", [Castling Black Kingside]),
        ("e1", [Castling White Kingside, Castling White Queenside]),
        ("e8", [Castling Black Kingside, Castling Black Queenside])]
