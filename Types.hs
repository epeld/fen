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
