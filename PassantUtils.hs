module PassantUtils where
import Control.Monad
import Control.Monad.Reader

import UpdateFunctions
import MoveDescription
import MoveType
import Square
import PositionReader

passantSquare :: UpdateReader (Maybe Square)
passantSquare = do
    mv <- asks move
    p <- asks originalPosition
    return $
        case mv of
            PawnMove _ _ -> 
                let d = destination mv
                in behind d `runReader` p
            _ -> Nothing

