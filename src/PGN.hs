module PGN where
import Control.Monad

import LegalMove

data Error = Ambiguous [FullMove] | Invalid

data Move = Standard PartialMove | Castles Side

play :: Move -> PReader (Either Error Position)
play (Castles s) = castle s
play (Standard mv) = move mv

-- foldr 'operand' to fold over a [Move] to produce a resulting position
playr :: Move -> Either Error Position -> Either Error Position
playr mv p = runReader (play mv) =<< p

move :: PartialMove -> PReader (Either Error Position)
move mv = do
    mvs <- fullMoves mv
    case mvs of
        [x] -> Right <$> after x
        [] -> return $ Left Invalid
        xs -> return $ Left $ Ambiguous xs

castle :: Side -> PReader (Either Error Position)
castle s = 
    if canCastle s
    then Right <$> afterCastles s
    else return $ Left Invalid
