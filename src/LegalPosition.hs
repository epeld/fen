module LegalPosition where
import Prelude ()
import Data.Eq
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Foldable
import Data.List (filter)
import Data.Int
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

error :: PReader (Maybe Error)
error = do
    ka <- kingAttackedErrors
    mp <- missingPromotionErrors
    return $ safeHead $ ka `mappend` mp
    

missingPromotionErrors :: PReader [Error]
missingPromotionErrors = fmap MissingPromotion `liftM` missingPromotions

kingAttackedErrors :: PReader [Error]
kingAttackedErrors = fmap KingAttacked `liftM` kingAttackers

missingPromotions :: PReader [Square]
missingPromotions = do
    c <- turn
    lr <- lastRank

    pawns <- pawnSquares c
    return $ mapMaybe (justRank lr) pawns


kingAttackers :: PReader [Square]
kingAttackers = do
    c <- turn
    pos <- ask

    let attackers sq = allAssailants c sq `runReader` pos

    sqs <- kingSquares c
    return $ concatMap attackers sqs


pawnSquares :: Color -> PReader [Square]
pawnSquares c = filterPieces (Piece Pawn c)


justRank :: Int -> Square -> Maybe Square
justRank r sq = if rank sq == r then Just sq else Nothing
