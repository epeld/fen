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

piecesColored c = Piece <$> [Pawn ..] <*> [c]

allAssailants :: Color -> Square -> PReader [Square]
allAssailants c sq = do
    as <- sequence $ assailants <$> piecesColored c <*> [sq]
    return (mconcat as)


assailants :: Piece -> Square -> PReader [Square]

assailants pawn@(Piece Pawn c) sq = 
    let ds = pawnAttackSources c
     in filterM (hasPiece pawn) (concat $ apply1 sq ds)

-- TODO king can only go 1 square
assailants piece@(Piece (Officer officer) c) sq = fmap catMaybes $ sequence $ fmap (firstPiece piece) $ apply sq (directions officer)


firstPiece :: Piece -> [Square] -> PReader (Maybe Square)
firstPiece p sqs = do
    pos <- ask
    msq <- firstNonEmpty sqs
    return $ mfilter (Position.hasPiece pos p) msq

firstNonEmpty :: [Square] -> PReader (Maybe Square)
firstNonEmpty sqs = do
    pos <- ask
    return $ safeHead $ dropWhile (isNothing. lookupSquare pos) sqs




safeHead [] = Nothing
safeHead xs = Just (head xs)

lookupSquare pos sq = runReader (pieceAt sq) pos
