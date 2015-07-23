module LegalMove where

-- TODO check that we can e.g capture on a square or move somewhere, and if not return error?


-- Figure out all the legal moves that can be made, given a partially specified move and a position
fullMoves :: Qualifier src => Move src -> PReader [FullMove]
fullMoves mv = do
    cands <- candidates mv
    let mvs = fullMove mv <$> cands
    filterM isLegal mvs




isLegal :: FullMove -> PReader Bool
isLegal mv = isNothing <$> error mv




error :: FullMove -> PReader (Maybe LP.Error)
error mv = local (runReader $ after mv) LP.error
