module LegalMove where
import Prelude (undefined)
import Data.Eq
import Text.Show

import MoveDescription
import qualified PartialDescription as Partial
import Square
import MoveType
import PositionReader
import FullMove

{-
fullMoves :: MoveDescription desc => Move desc -> PReader [FullMove]
fullMoves mv = do
    mvs <- fmap fullMove `liftM` candidates mv
    filterM legal mvs


legal :: FullMove -> PReader Bool
legal mv = local (runReader $ after mv) LegalPosition.legal

-}
