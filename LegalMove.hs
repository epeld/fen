module LegalMove where
import MoveTypes

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

