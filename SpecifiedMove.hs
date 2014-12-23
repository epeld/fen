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
findPieces (Move d) = candidates d
               

specified :: Move -> Square -> PReader (Maybe SpecifiedMove)
specified mv sq = do
    p <- pieceAt sq
    let pt = fmap pieceType p
        smv = SpecifiedMove <$> pt <*> Just mv <*> Just sq
    return smv

