module FullMove where
import Control.Applicative
import Control.Lens

import Square
import Move
import MoveQualifier

newtype FullMove = Full (Move Square) deriving (Show, Eq)

instance Qualifier Square where
    qualifies = (==)
    

-- Make Move an instance of Qualifier when applicable.
-- This allows us to filter out non-qualifying moves by
-- mv `qualifier` src
instance (Qualifier a) => Qualifier (Move a) where
    qualifies mv = qualifies (mv ^. source)


-- promote a qualifying move to a full, when possible
promote :: (Qualifier src) => Move src -> Square -> Maybe FullMove
promote mv sq = do
    sqr <- qualify mv sq
    return $ Full $ source .~ sqr $ mv

