module SpecifiedMove where
import Prelude ()
import Data.Bool
import Control.Monad

import Square
import MoveTypes
import MoveUtils
import Position

specify :: Move -> PReader [SpecifiedMove]
specify mv = do
    sqs <- findPieces mv
    filterM valid (Specified mv `fmap` sqs)


valid :: SpecifiedMove -> PReader Bool
valid smv = case classify smv of
    Left pmv -> validPawnMove pmv
    Right omv -> validOfficerMove omv


after :: SpecifiedMove -> PReader Position

