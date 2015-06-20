module LegalMove where
import Prelude ()
import Data.Eq
import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.List (filter)
import Control.Applicative
import Control.Monad.Plus
import Control.Monad.Reader
import Text.Show

import MoveDescription
import qualified PartialDescription as Partial
import Square
import MoveType
import PositionReader
import FullMove
import Candidates
import UpdatedPosition
import qualified LegalPosition as LP
import Move


fullMoves :: MoveDescription desc => Move desc -> PReader [FullMove]
fullMoves mv = do
    cands <- candidates (description mv)
    let mvs = fmap (fullMove mv) cands
    filterM isLegal mvs




isLegal :: FullMove -> PReader Bool
isLegal mv = fmap isNothing $ error mv




error :: FullMove -> PReader (Maybe LP.Error)
error mv = local (runReader $ after mv) LP.error
