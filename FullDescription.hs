module FullDescription (fullMove, Description(Description), MoveError, FullMove, source) where
import Prelude (undefined)
import Data.Eq
import Text.Show

import MoveDescription
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

instance MoveDescription Description where
    destination = FullDescription.destination
    moveType = FullDescription.moveType

type FullMove = Move Description

fullMove :: MoveDescription desc => Move desc -> Square -> FullMove
fullMove mv src =
    let desc = Description { source = src,
                             FullDescription.destination = MoveDescription.destination mv, 
                             FullDescription.moveType = MoveDescription.moveType mv }
    in
    case mv of
        PawnMove d promo -> PawnMove desc promo
        OfficerMove ot d -> OfficerMove ot desc
