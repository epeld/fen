module Game where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Applicative
import qualified Board 
import Piece
import Data.Maybe
import Square

data Side = Kingside | Queenside deriving (Show, Eq)
data CastlingRight = CastlingRight Side Color deriving (Show, Eq)

