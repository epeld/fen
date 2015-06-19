module MonadUtils where
import Prelude ()

import Data.Maybe
import Data.Function
import Control.Monad as M

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = liftM catMaybes $ mapM f xs
