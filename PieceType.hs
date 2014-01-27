module PieceType where
import Data.Maybe

data PieceType = Pawn | Officer OfficerType deriving (Show, Eq)

data OfficerType = Bishop | Knight | Rook | Queen | King deriving (Show, Eq)


officerCharMap =
 [('r', Rook),
  ('n', Knight),
  ('b', Bishop),
  ('q', Queen),
  ('k', King)]


charOfficerMap = reverseLookup officerCharMap


officerType :: Char -> Maybe OfficerType
officerType c = lookup (toLower c) officerCharMap


pieceType :: Char -> Maybe PieceType
pieceType c = case (toLower c) of
  'p' -> Just Pawn
  _ -> Officer `fmap` (officerChar c)


officerChar :: OfficerType -> Char
officerChar ot = fromJust (lookup ot charOfficerMap)


char pt = case pt of
  Pawn -> 'p'
  _ -> Officer $ officerChar c
