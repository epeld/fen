module FullMove where
import Control.Applicative
import Control.Lens

import Square
import Move
import MoveQualifier

newtype FullMove = Full (Move Square) deriving (Show, Eq)

instance Qualifier Square where
    qualifies = (==)
    

-- promote a qualifying move to a full, when possible
promote :: (Qualifier src) => Move src -> Square -> Maybe FullMove
promote mv sq = do
    sqr <- qualify mv sq
    return $ Full $ source .~ sqr $ mv

