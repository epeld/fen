module Assailants where
import Prelude ()
import Control.Applicative
import Control.Monad
import Data.Function
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Eq
import Control.Monad.Reader

import PositionReader
import qualified Position
import Piece
import Square
import Movement
import SquareListUtils
import Directions


allAssailants :: Color -> Square -> PReader [Square]

allAssailants c sq = do
    let as = assailants <$> allPiecesColored c <*> [sq]
    mconcat `fmap` sequence as


assailants :: Piece -> Square -> PReader [Square]

assailants pawn@(Piece Pawn c) sq = 
    let ds = pawnAttackSources c
     in filterM (hasPiece pawn) (concat $ apply1 sq ds)

assailants piece@(Piece (Officer officer) c) sq = do
    first <- mapM (firstPiece piece) (officerSquares officer sq)
    return (catMaybes first)



allPiecesColored c = Piece <$> [Pawn ..] <*> [c]
