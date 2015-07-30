module PositionUpdates where
import Control.Applicative
import Control.Monad.Reader
import Control.Lens

import Position
import FullMove
import BoardUpdates
import PropertiesUpdates
import ListUtils


after :: Position -> FullMove -> Position
after p mv = Position { _board = b', _properties = props }
    where
    b' = apply BoardUpdates.updates `runReader` (mv, p ^. board)
    props = apply PropertiesUpdates.updates `runReader` (mv, p ^. properties)


apply :: [Reader (FullMove, a) (a -> a)] -> Reader (FullMove, a) a
apply updates = do
    a <- asks snd
    compose <$> sequence updates <*> pure a
