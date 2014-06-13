module Utils where
import Control.Monad (liftM, filterM, (>>=), (=<<))
import Control.Monad.Trans.Reader (runReader, ReaderT, ask, local)
import Control.Applicative ((<$>), (<*>), pure, Applicative)
import Data.Monoid (mappend, mconcat, First(..), getFirst, Monoid)
import Types (PositionReader, Error)

findElem :: a -> [a] -> Maybe a
findElem a = find (== a)

maybeNot :: Monad m => (a -> m Bool) -> Maybe a -> m Bool
maybeNot = maybe (return False)

safeHead [] = Nothing
safeHead xs = Just $ head xs

maybeError :: Maybe a -> b -> Either a b
maybeError Nothing a = Right a
maybeError (Just err) _ = Left err

isRight (Right _) = True
isRight (Left _) = False

flipMaybe a b = case b of
    Nothing -> Just a
    Just _ -> Nothing

firstError :: [PositionReader (Maybe Error)] -> PositionReader (Maybe Error)
firstError = liftM (getFirst. mconcat. map First). sequence

justWhen :: Bool -> a -> Maybe a
justWhen a b = if a then Just b else Nothing

justUnless :: Bool -> a -> Maybe a
justUnless a = justWhen (not a)

justWhenM1 :: Applicative f => f Bool -> a -> f (Maybe a)
justWhenM1 a b = justWhen <$> a <*> pure b

justUnlessM1 :: Applicative f => f Bool -> a -> f (Maybe a)
justUnlessM1 a b = justUnless <$> a <*> pure b

if_ :: Bool -> a -> a -> a
if_ a b c = if a then b else c

ifM1 :: Applicative f => f Bool -> a -> a -> f a
ifM1 a b c = if_ <$> a <*> pure b <*> pure c

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM f xs = liftM (flip take xs. length. takeWhile id) $ mapM f xs

anyM :: (a -> PositionReader Bool) -> [a] -> PositionReader Bool
anyM f [] = return True
anyM f xs = liftM (not. null) (filterM f xs)

everyM :: [PositionReader Bool] -> PositionReader Bool
everyM = fmap and. sequence

someM :: [PositionReader Bool] -> PositionReader Bool
someM = anyM id

-- The holy grail:
swapM :: (a -> PositionReader b) -> PositionReader (a -> b)
swapM r = liftM (\x -> liftM (`runReader` x) r) ask
