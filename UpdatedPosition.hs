module UpdatedPosition where
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
import Square
import Piece
import MoveType
import qualified Position
import Position (Position)
import PositionReader
import FullMove
import UpdateFunctions
import BoardUpdates
import PropertiesUpdates


after :: FullMove -> PReader Position
after mv = do
    p <- ask
    let env = UpdateEnvironment { move = mv, originalPosition = p }
    return $ runReader newPosition env


newPosition :: UpdateReader Position
newPosition = positionUpdateFn `ap` asks originalPosition

positionUpdateFn :: UpdateReader UpdateFn
positionUpdateFn = liftM compose updateStack


-- The update stack has two substacks: board and properties
-- defined in BoardUpdates and PropertiesUpdates
updateStack :: UpdateReader [UpdateFn]
updateStack = do
    bs <- boardStack 
    ps <- propertiesStack
    return (bs ++ ps)
