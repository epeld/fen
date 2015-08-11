module FullMove where
import Control.Applicative
import Control.Lens

import Square
import Move
import MoveQualifier

type FullMove = Move Square


-- promote a qualifying move to a full, when possible
promote :: (Qualifier src) => Move src -> Square -> Maybe FullMove
promote mv sq = do
    sqr <- qualify mv sq
    return $ source .~ sqr $ mv

