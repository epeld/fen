module Types where
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Reader (ReaderT)

data Error = KingCapturable |
             NoPiece |
             WrongColor |
             NotInRange |
             MissingKing |
             IllegalPromotion |
             PromotionRequired
             deriving (Show, Eq)

data Piece = Piece { pieceType :: PieceType, color :: Color }
             deriving (Show, Eq)

data PieceType = Pawn |
                 Officer OfficerType
                 deriving (Show, Eq)

data OfficerType = Knight | Bishop | Rook | Queen | King
                   deriving (Show, Eq, Ord, Enum)


data Move = Move { source :: Square,
                   destination :: Square,
                   promotion :: Maybe OfficerType }
            deriving (Show, Eq)
