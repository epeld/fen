module Move where
import Prelude ()
import Control.Monad as M
import Data.Either

import qualified Position
import MonadUtils

data Move = Move

data MoveError = Ambiguous [SpecifiedMove] | Invalid deriving (Show, Eq)

-- Specified Move: guarantees that there is a piece at its source square
data SpecifiedMove = Specified Move Square deriving (Show, Eq)

-- LegalMove: guarantees that the move is legal and results in a legal position
data Legal a = Legal a
type LegalMove = Legal SpecifiedMove

-- SpecifiedPawnMove: guarantees that the moved piece is a pawn
newtype SpecifiedPawnMove = PawnMove SpecifiedMove

-- SpecifiedOfficerMove: guarantees that the moved piece is an officer
newtype SpecifiedOfficerMove = OfficerMove SpecifiedMove


legalize :: Move -> Position.PReader (Either MoveError LegalMove)
legalize mv = do
    mvs <- legalize' mv
    case mvs of
        [mv] -> Right mv
        [] -> Left Invalid
        mvs -> Left $ Ambiguous mvs

legalize' :: Move -> Position.PReader [LegalMove]
legalize' = specify >=> mapMaybeM legalize

legalize :: SpecifiedMove -> Position.PReader (Maybe LegalMove)
legalize smv = do
    p <- legalAfter smv
    case p of
        Nothing -> Nothing
        Just _ -> Just (Legal smv)
    

specify :: Move -> PositionReader [SpecifiedMove]
specify mv = do
    sqs <- findPieces mv
    let smvs = fmap (Specified mv) sqs
    filterM valid smvs

pawnMove :: SpecifiedMove -> Maybe SpecifiedPawnMove
pawnMove smv = case piece smv of
    Pawn _ -> Just $ SpecifiedPawnMove smv
    _ -> Nothing

officerMove :: SpecifiedMove -> Maybe SpecifiedOfficerMove
officerMove smv = case piece smv of
    Officer smv -> Just $ SpecifiedOfficerMove smv
    _ -> Nothing

classify :: SpecifiedMove -> Either SpecifiedPawnMove SpecifiedOfficerMove
classify smv = case first of
    Just x -> x
    _ -> error "Invalid Specified Move; couldn't classify"
    where first = getFirst $ mconcat $ fmap First $ [pawnMove, officerMove] <*> [smv]

valid :: SpecifiedMove -> PositionReader Bool
valid smv = case classify smv of
    Left pmv -> validPawnMove pmv
    Right omv -> validOfficerMove omv

findPieces :: Move -> PositionReader [Square]
findPieces = piece >=> filterPieces

piece :: Move -> PositionReader Piece

filterPieces :: Piece -> PositionReader [Square]

legal :: SpecifiedMove -> PositionReader Bool
legal = fmap legal. after

after :: SpecifiedMove -> PositionReader Position

move :: LegalMove -> PositionReader Position
