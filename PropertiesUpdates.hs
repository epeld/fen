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
propertiesStack = sequence [fullMoveUpdateFn,
                            halfMoveUpdateFn,
                            castlingRightsUpdateFn,
                            passantSquareUpdateFn]


fullMoveUpdateFn :: UpdateReader UpdateFn
fullMoveUpdateFn = do
    color <- turn
    return $ case color of
        Black -> \p -> p { fullMoveCount = inc (fullMoveCount p) }
        White -> id


halfMoveUpdateFn :: UpdateReader UpdateFn
halfMoveUpdateFn = return $ 
    \p -> p { halfMoveCount = inc (halfMoveCount p) }


castlingRightsUpdateFn :: UpdateReader UpdateFn
castlingRightsUpdateFn = do
    mv <- asks move
    return id -- TODO


passantSquareUpdateFn :: UpdateReader UpdateFn
passantSquareUpdateFn = do
    return id -- TODO
