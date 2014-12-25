module PropertiesUpdates where
import Prelude (undefined)
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
import FullDescription
import UpdateFunctions


-- The properties stack contains all updateFns that will update the position's meta info (e.g move count etc)
propertiesStack :: UpdateReader [UpdateFn]
propertiesStack = sequence [fullMove, halfMove, castlingRights, passantSquare]


fullMove :: UpdateReader UpdateFn
fullMove = do
    color <- turn
    return $ case color of
        Black -> \p -> p { fullMoveCount = inc (fullMoveCount p) }
        White -> id


halfMove :: UpdateReader UpdateFn
halfMove = return $ 
    \p -> p { halfMoveCount = inc (halfMoveCount p) }


castlingRights :: UpdateReader UpdateFn
castlingRights = do
    mv <- asks move
    return id -- TODO


passantSquare :: UpdateReader UpdateFn
passantSquare = do
    return id -- TODO


newTurn :: UpdateReader UpdateFn
newTurn = return $ \p -> p { turn = otherColor (turn p) }
