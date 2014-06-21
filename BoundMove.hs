module BoundMove where
import Control.Monad (liftM2)

import Data.Monoid (mconcat)
import Data.Maybe (isNothing)

import qualified Position
import Square (Square, file, rank)
import Move (Promotion)
import PieceLike (PieceLike)
import BoundPiece (Piece)

data Move = Move { boundPiece :: Piece,
                   destination :: Square,
                   promotion :: Promotion }
            deriving (Show, Eq)

instance PieceLike Move where
  piece = piece. boundPiece
  square = square. boundPiece
  position = position. boundPiece

move :: Piece -> Square -> Promotion -> Either Error Move
move pc dst pm =
  let mv = Move pc dst pm
      check = first. sequence moveCheckers
  in case check mv of
    Just err -> Left err
    Nothing -> Right mv

moveCheckers = [colorChecker, promotionChecker, rangeChecker, checkChecker]

type MoveChecker = (Move -> Maybe Error)

colorChecker :: MoveChecker
colorChecker = checker WrongColor isFriendly

promotionChecker :: MoveChecker
promotionChecker = checker IllegalPromotion isPromotingCorrectly

isPromotingCorrectly :: Move -> Bool
isPromotingCorrectly mv = shouldPromote mv == hasPromotion mv

shouldPromote :: Move -> Bool
shouldPromote mv = isPawn mv && isLastRankDestination mv

isLastRankDestination mv = Position.lastRank (position mv) == rank (destination mv)

hasPromotion :: Move -> Bool
hasPromotion (Valid _ _ pm) = not $ isNothing pm

type Checker = (a -> Maybe Error)

checker :: (a -> Bool) -> Error -> Checker
checker pred err a = if pred a then Nothing else Just err

first :: [Maybe a] -> Maybe a
first = getFirst. mconcat. map First
