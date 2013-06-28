module Threats (enemyKingIsSafe, squareIsThreatened, squareIsDefended) where
import Data.Either (rights, lefts)
import Control.Applicative ((<$>))

import ProjectedRange (threatens,)
import MovingPiece (movingPiece,)
import Piece (PieceType(..), OfficerType(King), Piece(..), )
import Position (friendlySquares, enemySquares, enemy)

squareIsThreatened p sq = not. any (threatens sq) $ enemies p
squareIsDefended p sq = not. any (threatens sq) $ friendlies p

enemyKingIsSafe p = squareIsDefended p enemyKingSq
    where enemyKingSq = enemy (Officer King) p

enemies p = shouldntFail $ movingPiece p <$> enemySquares p
friendlies p = shouldntFail $ movingPiece p <$> friendlySquares p

shouldntFail mps = case lefts mps of
    [] -> rights mps
    x -> error $ show $ head mps
