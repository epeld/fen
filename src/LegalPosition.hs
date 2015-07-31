module LegalPosition where
import qualified Data.Map as Map
import Control.Monad.Plus
import Control.Lens
import Data.Maybe

import Candidates
import Square
import Piece
import ListUtils
import Position

data Error = MissingPromotion Square | KingCapturable Square
             deriving (Show, Eq)


legalize :: Position -> Maybe Position
legalize = partial isLegal
    where
    isLegal = null . errors


errors :: Position -> [Error]
errors p = map KingCapturable (kingAttackers p) ++ map MissingPromotion (missingPromotions p)
    

kingAttackers :: Position -> [Square]
kingAttackers p = attackers p `concatMap` enemyKingSquares
    where
    enemyKingSquares = filterSquares p enemyKing
    enemyKing = Piece (Officer King) (p ^. enemyColor)


missingPromotions :: Position -> [Square]
missingPromotions p = partial isLastRank `mapMaybe` friendlyPawnSquares
    where
    isLastRank sq = rank sq == lastRank (p ^. turn)
    friendlyPawnSquares = filterSquares p friendlyPawn
    friendlyPawn = Piece Pawn (p ^. friendlyColor)


filterSquares p pred = Map.keys $ filterPieces p pred
