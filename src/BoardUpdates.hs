module BoardUpdates where
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.Maybe

import ListUtils
import Square
import Piece
import MoveType
import Position
import Move
import FullMove

type Update = Board -> Board
type UpdateReader = Reader (FullMove, Board)


updates :: [UpdateReader Update]
updates = [movePiece, BoardUpdates.passant, BoardUpdates.promotion]


movePiece :: UpdateReader Update
movePiece = do
    (mv, b) <- ask

    let src = mv ^. source
        dst = mv ^. destination

    return $ (at src .~ Nothing) . (at dst .~ (b ^. at src))


passant :: UpdateReader Update
passant = do
    (mv, b) <- ask

    -- Passant is a pawn capture on an empty square
    let isPassant = isPawnMove mv && isCapture mv && Nothing == (b ^. at (mv ^. destination))

    return $ if isPassant then deletePassantedPawn mv else id
        

promotion :: UpdateReader Update
promotion = do
    (mv, b) <- ask

    return $ case join (mv ^? Move.promotion) of
        Nothing -> id
        Just ot -> at (mv ^. destination) %~ (mapped . Piece.pieceType .~ Officer ot)


-- Note: we don't actually check that a pawn is deleted, but if the rules are followed it should be
deletePassantedPawn :: FullMove -> Update
deletePassantedPawn mv = case passantSquare mv of
    Nothing -> id
    Just sq -> Map.delete sq


passantSquare :: FullMove -> Maybe Square
passantSquare mv = add (mv ^. source) (off & _2 .~ 0) -- only x-component!
    where
    off = (mv ^. destination) `diff` (mv ^. source)
