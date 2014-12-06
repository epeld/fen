module Move where
import Prelude ()
import Control.Monad as M
import Data.Either

import qualified Position
import MonadUtils

legalize :: Move -> Position.PReader (Either MoveError LegalMove)
legalize mv = do
    mvs <- legalize' mv
    case mvs of
        [mv] -> Right mv
        [] -> Left Invalid
        mvs -> Left $ Ambiguous mvs

legalize' :: Move -> Position.PReader [LegalMove]
legalize' = specify >=> mapMaybeM legalize

legalize :: SpecifiedMove -> Position.PReader (Maybe LegalMove)
legalize smv = do
    p <- legalAfter smv
    case p of
        Nothing -> Nothing
        Just _ -> Just (Legal smv)

