module FullDescription where
import Prelude (undefined)
import Data.Eq
import Text.Show

import MoveDescription
import qualified PartialDescription as Partial
import Square
import MoveType
import PositionReader

data MoveError = Ambiguous [FullMove] | Invalid

data Description = Description {
        source :: Square,
        destination :: Square, 
        moveType :: MoveType
    } deriving (Show)

instance MoveDescription Description where
    destination = FullDescription.destination
    moveType = FullDescription.moveType

newtype FullMove = Move Description



-- TODO move these two fns to a Legal-module
fullMoves :: MoveDescription desc => Move desc -> PReader [FullMove]
fullMoves mv = do
    mvs <- fmap fullMove `liftM` candidates mv
    filterM legal mvs


legal :: FullMove -> PReader Bool
legal mv = local (runReader $ after mv) Position.legal



-- TODO put everything below this line into new module: UpdatePosition or something
after :: FullMove -> PReader Position
after mv = do
    p <- ask
    let env = UpdateEnvironment { move = mv, originalPosition = p }
    return $ runReader newPosition env

-- Represents the information needed to create a new position from an old
-- e.g we need to know how the position looked initially, and we need to know what is going to change
data UpdateEnvironment = UpdateEnvironment { move :: FullMove, originalPosition :: Position }
type UpdateProcessor = Reader UpdateEnvironment

-- We will create a new position from an old one by generating lots of small update functions,
-- each updating a single property of the position, and then composing them
type UpdateFn = Position -> Position

newPosition :: UpdateProcessor Position
newPosition = positionUpdater `ap` asks originalPostition

positionUpdater :: UpdateProcessor UpdateFn
positionUpdater = composeAll `ap` updaterStack

-- TODO move to utils
composeAll :: [(a -> a)] -> a -> a
composeAll = appEndo. foldMap Endo

-- The update stack has two substacks: board and properties
updaterStack :: UpdateProcessor [UpdateFn]
updaterStack = do
    bs <- boardStack 
    ps <- propertiesStack
    return (bs ++ ps)

-- The board stack contains all updaters that will update the position's pieces
boardStack :: UpdateProcessor [UpdateFn]
boardStack = sequence [movePieceUpdater, passantUpdater, promotionUpdater]

-- The properties stack contains all updaters that will update the position's meta info (e.g move count etc)
propertiesStack :: UpdateProcessor [UpdateFn]
propertiesStack = return [] -- TODO

movePieceUpdater :: UpdateProcessor UpdateFn
movePieceUpdater = do
    mv <- move `fmap` ask
    return $ movePiece (source mv) (destination mv)

passantUpdater :: UpdateProcessor UpdateFn
passantUpdater orig = do
    orig <- originalPosition `fmap` ask
    mv <- move `fmap` ask
    if isPassant mv orig 
    then return p -- TODO delete passanted pawn
    else return p

promotionUpdater :: UpdateProcessor UpdateFn
promotionUpdater = do
    orig <- originalPosition `fmap` ask
    mv <- move `fmap` ask
    return $ case mv of
        (PawnMove desc (Just officer)) -> 
            let dst = destination mv
                piece = Piece (Officer officer) (turn orig)
             in \p -> p { board = insert dst piece (board p) }

        _ -> id


    

fullMove :: MoveDescription desc => Square -> Move desc -> FullMove
fullMove mv src =
    let desc = Description { source mv = src,
                             destination = MoveDescription.destination mv,  -- TODO desc instead of mv somehow
                             moveType = MoveDescription.moveType mv }
    in FullMove desc
