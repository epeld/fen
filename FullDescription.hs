module FullDescription (fullMove, Description(Description), MoveError, FullMove, source, passantSquare) where
import Prelude (undefined)
import Data.Eq
import Data.Maybe
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
    destination = FullDescription.destination
    moveType = FullDescription.moveType

type FullMove = Move Description

fullMove :: MoveDescription.MoveDescription desc => Move desc -> Square -> FullMove
fullMove mv src =
    let desc = Description { source = src,
                             FullDescription.destination = MoveDescription.destination mv, 
                             FullDescription.moveType = MoveDescription.moveType mv }
    in
    case mv of
        PawnMove d promo -> PawnMove desc promo
        OfficerMove ot d -> OfficerMove ot desc


-- TODO consider moving an not using PReader (use UpdateReader instead!)
-- Change is not super easy, though because 'behind' also is a PReader
passantSquare :: FullMove -> PReader (Maybe Square)
passantSquare mv@(PawnMove _ _) =
    let dst = destination (description mv)
        (_, y) = dst `diff` source (description mv)
    in 
    if y == 2 
    then behind dst 
    else return Nothing
