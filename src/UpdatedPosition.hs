module UpdatedPosition where
import Control.Monad
import Control.Monad.Reader

import ListUtils
import Square
import Piece
import MoveType
import Position
import FullMove

import UpdateFunctions
import BoardUpdates
import PropertiesUpdates


after :: Position -> FullMove -> Position
after p mv = newPosition `runReader` UpdateEnvironment { move = mv, originalPosition = p }


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
