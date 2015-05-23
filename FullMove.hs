module FullMove (fullMove, Description(Description), 
                  MoveError, FullMove, source, fullSource) where
import Prelude (undefined)
import Data.Eq
import Data.Maybe
import Data.Function
import Control.Monad (return)
import Text.Show

import qualified MoveDescription 
import qualified PartialDescription as Partial
import Square
import MoveType
import PositionReader

data MoveError = Ambiguous [FullMove] | Invalid

data Description = Description {
        source :: Square,
        destination :: Square, 
        moveType :: MoveType
    } deriving (Show)

instance MoveDescription.MoveDescription Description where
    destination = FullMove.destination
    moveType = FullMove.moveType
    possibleSource desc sq = sq == (source desc)

type FullMove = Move Description

fullMove :: MoveDescription.MoveDescription desc => Move desc -> Square -> FullMove
fullMove mv src =
    let desc = Description { source = src,
                             FullMove.destination = MoveDescription.destination mv, 
                             FullMove.moveType = MoveDescription.moveType mv }
    in
    case mv of
        PawnMove d promo -> PawnMove desc promo
        OfficerMove ot d -> OfficerMove ot desc

fullSource :: FullMove -> Square
fullSource mv = source $ description mv
