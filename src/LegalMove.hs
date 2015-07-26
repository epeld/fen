module LegalMove where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except


data Error = NoCandidate | Ambiguous [FullMove] | InvalidMoveType MoveType derviving (Show, Eq)


type LegalityFunction a = Position -> PartialMove -> Except (NonEmpty Error) a
type Verifier :: LegalityFunction  ()


fullMove :: LegalityFunction FullMove
fullMove mv = do
    mvs <- fullMoves mv
    disambiguate mvs


-- Figure out all the legal moves that can be made, given a partially specified move and a position
fullMoves :: LegalityFunction [FullMove]
fullMoves p mv = do
    verifyMoveType p mv

    let legal = isLegal p
        mvs = promote mv <$> cands
        cands = candidates p mv

    return (filter legal mvs)


verifyMoveType :: Verifier
verifyMoveType p mv = 
    case mv ^. moveType of
        Captures -> verifyStandardCapture p mv <|> verifyPassantCapture p mv
        Moves -> verifyMove p mv


verifyStandardCapture :: Verifier
verifyStandardCapture p mv =
   if p ^. board . at (mv ^. destination) . color == Just (p ^. turn . to otherColor)
   then pass
   else throwE (InvalidMoveType Captures)


verifyPassantCapture :: Verifier
verifyPassantCapture p mv =
    if p ^. board . at (mv ^. destination) == Nothing &&
       p ^. passant == Just (mv ^. destination) &&
       mv ^. moveType == Captures &&
       mv ^. pieceType == Pawn
    then pass
    else throwE (InvalidMoveType Captures)


verifyNoCapture :: Verifier
verifyNoCapture p mv =
    if p ^. board . at (mv ^. destination) == Nothing
    then pass
    else throwE (InvalidMoveType Moves)


isLegal :: Position -> FullMove -> Bool
isLegal p mv = isNothing (LegalPosition.error p')
    where 
    p' = after mv p


disambiguate :: [FullMove] -> Except (NonEmpty Error) FullMove
disambiguate [] = Left NoCandidates
disambiguate [mv] = Right mv
disambiguate mvs = Left $ Ambiguous mvs


pass = return ()
