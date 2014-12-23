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



fullMoves :: MoveDescription desc => Move desc -> PReader [FullMove]
fullMoves mv = do
    mvs <- fmap fullMove `liftM` candidates mv
    filterM legal mvs


legal :: FullMove -> PReader Bool
legal mv = local (runReader $ after mv) Position.legal



-- TODO put everything below this line into new module!
after :: FullMove -> PReader Position
after mv = do
    p <- ask
    let env = UpdateEnvironment { move = mv, originalPosition = p }
    return $ runReader newPosition env

data UpdateEnvironment = UpdateEnvironment { move :: FullMove, originalPosition :: Position }
type UpdateProcessor = Reader UpdateEnvironment
type UpdateFn = Position -> Position

newPosition :: UpdateProcessor Position
newPosition = positionUpdater `ap` asks originalPostition

positionUpdater :: UpdateProcessor UpdateFn
positionUpdater = composeAll `ap` updaterStack

-- TODO move to utils
composeAll :: [(a -> a)] -> a -> a
composeAll = appEndo. foldMap Endo

updaterStack :: UpdateProcessor [UpdateFn]
updaterStack = sequence $ concat [boardStack, propertiesStack]

boardStack :: [UpdateProcessor UpdateFn]
boardStack = [movePieceUpdater, passantUpdater, promotionUpdater]

propertiesStack :: [UpdateProcessor UpdateFn]
propertiesStack = [] -- TODO

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
    if isLastRank mv orig 
    then return id -- TODO promote 
    else return id



    

fullMove :: MoveDescription desc => Square -> Move desc -> FullMove
fullMove mv src =
    let desc = Description { source mv = src,
                             destination = MoveDescription.destination mv,  -- TODO desc instead of mv somehow
                             moveType = MoveDescription.moveType mv }
    in FullMove desc
