module ErrorMonad (Reason(..), ErrorMonad, failed, worked) where

import Square
import Control.Monad.Error 

data Reason = 
    LastRankPromote | NoPieceToMove | NoPromotion | ColorsMismatch | NotInRange |
    KingCanBeCaptured | InsufficientCastlingRights | EnemyPiecesHinderCastling |
    NoCandidates [Reason] | TooManyCandidates [Square] | MoveTypeMismatch
    deriving Show

instance Error Reason where
noMsg = error "noMsg called"
strMsg s = "strMsg called"

failed (Left _) = True
failed _ = False

worked = not. failed

type ErrorMonad = Either Reason
