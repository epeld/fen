module SpecifiedMove where
import Prelude ()

import MoveTypes
import Position

specify :: Move -> PositionReader [SpecifiedMove]
specify mv = do
    sqs <- findPieces mv
    let smvs = fmap (Specified mv) sqs
    filterM valid smvs

findPieces :: Move -> PositionReader [Square]
findPieces = piece >=> filterPieces

valid :: SpecifiedMove -> PositionReader Bool
valid smv = case classify smv of
    Left pmv -> validPawnMove pmv
    Right omv -> validOfficerMove omv


legal :: SpecifiedMove -> PositionReader Bool
legal = fmap legal. after

after :: SpecifiedMove -> PositionReader Position

move :: LegalMove -> PositionReader Position
