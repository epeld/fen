module Move where
import Piece (OfficerType)
import Square (Square)

type Promotion = Maybe OfficerType

data Move = Move { source :: Square,
                   destination :: Square,
                   promotion :: Promotion }
            deriving (Show, Eq)
