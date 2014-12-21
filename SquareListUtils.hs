module SquareListUtils where
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

-- TODO move to Position
lookupSquare pos sq = runReader (pieceAt sq) pos
