module LegalPosition where
import Prelude ()
import Data.Eq
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.List (filter)
import Control.Applicative
import Control.Monad.Plus
import Control.Monad.Reader
import Text.Show

import PositionReader
import Candidates
import Square
import Piece
import ListUtils

data Error = MissingPromotion Square | KingAttacked Square
             deriving (Show, Eq)


legal :: PReader (Maybe Error)
legal = do
    ka <- kingAttacked
    mp <- missingPromotion
    return $ getFirst $ ka `mappend` mp



missingPromotion :: PReader (First Error)
missingPromotion = do
    c <- turn
    lr <- lastRank
    pawns <- filterPieces (Piece Pawn c)

    let errors = filter (\sq -> rank sq == lr) pawns

    return $
        First $
        case errors of
            [] -> Nothing
            (err:_) -> Just (MissingPromotion err)



kingAttacked :: PReader (First Error)
kingAttacked = do
    c <- turn
    pos <- ask
    kingSqs <- kingSquares c

    let attackers :: Square -> [Square]
        attackers sq = runReader (allAssailants c sq) pos
        sqs = mconcat $ fmap attackers kingSqs

    return $ 
        First $ 
        case sqs of
            [] -> Nothing
            (sq:_) -> Just (KingAttacked sq)



