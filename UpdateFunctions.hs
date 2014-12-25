module UpdateFunctions where
import Prelude (undefined)
import Data.Eq
import Data.Function
import Data.Map
import Data.Maybe
import Data.Bool
import Text.Show
import Data.List ((++))
import Control.Monad
import Control.Monad.Reader

import ListUtils
import MoveDescription
import qualified PartialDescription as Partial
import Square
import Piece
import MoveType
import qualified Position
import Position (Position)
import PositionReader
import FullDescription

-- Represents the information needed to create a new position from an old
-- e.g we need to know how the position looked initially, and we need to know what is going to change
data UpdateEnvironment = UpdateEnvironment { move :: FullMove, originalPosition :: Position }

type UpdateReader = Reader UpdateEnvironment

-- We will create a new position from an old one by generating lots of small update functions,
-- each updating a single property of the position, and then composing them
type UpdateFn = Position -> Position



movePieceUpdateFn :: UpdateReader UpdateFn
movePieceUpdateFn = do
    mv <- asks move
    let src = source $ description mv
    return $ Position.movePiece src (destination mv)

passantUpdateFn :: UpdateReader UpdateFn
passantUpdateFn = do
    mv <- asks move
    msq <- asks $ runReader (passantedSquare mv). originalPosition
    return $ \p -> p { Position.board = deleteMaybe msq (Position.board p) }
        

promotionUpdateFn :: UpdateReader UpdateFn
promotionUpdateFn = do
    color <- asks (Position.turn. originalPosition)
    mv <- asks move
    return $ case mv of
        (PawnMove desc (Just officer)) -> 
            let dst = destination mv
                piece = Piece (Officer officer) color
             in \p -> p { Position.board = insert dst piece (Position.board p) }

        _ -> id


-- Returns the square of the pawn that was taken en passant
passantedSquare :: FullMove -> PReader (Maybe Square)
passantedSquare mv@(PawnMove (Description _ dst mt) _) = do
    ep <- asks Position.passant 
    color <- turn
    if ep == Just dst && mt == Captures
       then behind dst
       else return Nothing

passantedSquare _ = return Nothing




deleteMaybe :: Maybe Square -> Position.Board -> Position.Board
deleteMaybe = maybe id delete
