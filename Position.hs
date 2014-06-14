module Position where
import Color (Color)
import Move (Move(Move), Promotion)
import Piece (pieceType, color)

type Board = Map Square Piece

data Position = Position { board :: Board,
                           passant :: Maybe Square,
                           halfMoveNr :: Int,
                           fullMoveNr :: Int,
                           turn :: Color,
                           castling :: Set CastlingRight }
                           deriving (Show, Eq)

data Move = Valid Piece Square Promotion

lastRank :: Position -> Int
lastRank = Color.lastRank. turn

nextTurn :: Position -> Position
nextTurn p = p{turn = enemyColor p }

enemyColor :: Position -> Color
enemyColor = Color.toggle. turn

lookup :: Square -> Position -> Maybe Piece
lookup sq = Map.lookup sq. board

isEmpty :: Square -> Position -> Bool
isEmpty sq p = isNothing. lookup sq

--
-- Move Binding
--
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
colorChecker (Move p _ _) = checker WrongColor isFriendly

promotionChecker :: MoveChecker
promotionChecker = checker IllegalPromotion $
                   liftM2 (==) shouldPromote hasPromotion

shouldPromote :: Move -> Bool
shouldPromote mv = pieceType p == Pawn && isLastRankDestination mv

hasPromotion :: Move -> Bool
hasPromotion (Valid _ _ pm) = isJust pm

type Checker = (a -> Maybe Error)

checker :: (a -> Bool) -> Error -> Checker
checker pred err a = if pred a then Nothing else Just err

first :: [Maybe a] -> Maybe a
first = getFirst. mconcat. map First
