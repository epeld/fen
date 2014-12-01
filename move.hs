module Move where
import Control.Monad.Reader

data Position
data Move

type PositionReader = Reader Position
data MoveError = Ambiguous [SpecifiedMove] | Invalid deriving (Show, Eq)

-- Specified Move: guarantees that there is a piece at its source square
data SpecifiedMove = Specified Move Square deriving (Show, Eq)

-- LegalMove: guarantees that the move is legal and results in a legal position
newtype LegalMove = Legal SpecifiedMove

-- SpecifiedPawnMove: guarantees that the moved piece is a pawn
newtype SpecifiedPawnMove = PawnMove SpecifiedMove

-- SpecifiedOfficerMove: guarantees that the moved piece is an officer
newtype SpecifiedOfficerMove = OfficerMove SpecifiedMove


legalize :: Move -> PositionReader (Either MoveError LegalMove)
legalize mv = do
    cands <- candidates mv
    let lgl smv = fmap legal $ after smv
    fmap oneValid $ filterM smvs lgl

oneValid :: [SpecifiedMove] -> Either MoveError LegalMove
oneValid [mv] = Right mv
oneValid [] = Left Invalid
oneValid mvs = Left $ Ambiguous mvs
    
candidates :: Move -> PositionReader [SpecifiedMove]
candidates mv = do
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

legal :: Position -> Bool

after :: SpecifiedMove -> PositionReader PositionReader

move :: LegalMove -> PositionReader Position
