module BoardUpdates where
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader
import Control.Lens

import ListUtils
import Square
import Piece
import qualified MoveType
import qualified Position
import qualified Move
import Position 
import FullMove
import UpdateFunctions

-- The board stack contains all updateFns that will update the position's pieces
boardStack :: UpdateReader [UpdateFn]
boardStack = sequence [movePiece, passant, promotion]


movePiece :: UpdateReader UpdateFn
movePiece = do
    mv <- asks move
    let src = source $ Move.description mv
    return $ Position.movePiece src (destination mv)


passant :: UpdateReader UpdateFn
passant = do
    mv <- moveR
    orig <- originalPositionR
    let msq = runReader (passantedPawn mv) orig
    return $ \p -> p { Position.board = deleteMaybe msq (Position.board p) }
        

promotion :: UpdateReader UpdateFn
promotion = do
    color <- whoseMoveR
    mv <- moveR
    return $ case mv of
        (Move.PawnMove desc (Just officer)) -> 
            let dst = destination mv
                piece = Piece (Officer officer) color
             in \p -> p { Position.board = insert dst piece (Position.board p) }

        _ -> id



-- Util function:
-- Returns the square of the pawn that was taken en passant
passantedPawn :: FullMove -> PReader (Maybe Square)
passantedPawn mv@(Move.PawnMove (Description _ dst mt) _) = do
    ep <- asks Position.passant 
    color <- turn
    if ep == Just dst && mt == MoveType.Captures
       then behind dst
       else return Nothing

passantedPawn _ = return Nothing



-- Util funtion: delete the piece at the indicated square, or do nothing if passed Nothing
deleteMaybe :: Maybe Square -> Position.PieceMap -> Position.PieceMap
deleteMaybe Nothing = id
deleteMaybe (Just sq) = \ p -> p & board . at sq .~ Nothing
