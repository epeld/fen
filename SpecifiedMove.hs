module SpecifiedMove where
import Prelude ()
import Data.Bool
import Control.Monad
import Control.Applicative

import Square
import Position
import Candidates

specify :: Move -> PReader [SpecifiedMove]
specify mv = do
    sqs <- findPieces mv
    mvs <- fmap (specified mv) sqs
    filterM valid (catMaybes mvs)

findPieces :: PartialMove -> PReader [Square]
findPieces mv = 
               

specified :: Move -> Square -> PReader (Maybe SpecifiedMove)
specified mv sq = do
    p <- pieceAt sq
    let pt = fmap pieceType p
        smv = SpecifiedMove <$> pt <*> Just mv <*> Just sq
    return smv


valid :: SpecifiedMove -> PReader Bool
valid smv = valid' (
    where isValid

valid (SpecifiedMove Pawn mv sq) = undefined -- TODO
valid (SpecifiedMove (Officer off) mv sq) = undefined -- TODO

after :: SpecifiedMove -> PReader Position
after (SpecifiedMove Pawn mv sq) = undefined -- TODO
after (SpecifiedMove (Officer off) mv sq) = undefined -- TODO
