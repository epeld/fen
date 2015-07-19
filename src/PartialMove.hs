module PartialMove where
import Move
import MoveQualifier
import Square

type PartialMove = Move (Maybe PartialSquare)

data PartialSquare = Rank Int | File Char | Whole Square deriving Show

instance Qualifier PartialSquare where
    qualifies (Rank r) sq = rank sq == r
    qualifies (File f) sq = Just (file sq) == fileIndex f
    qualifies (Whole sq2) sq = sq == sq2

