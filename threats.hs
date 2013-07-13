module Threats (enemyKingIsSafe, squareIsThreatened, squareIsDefended) where
import Control.Applicative ((<$>))

import ProjectedRange (threatens,)
import MovingPiece (friendlies, enemies)
import Piece (PieceType(..), OfficerType(King), Piece(..), )
import Position (friendlySquares, enemySquares, enemy)

squareIsThreatened p sq = any (threatens sq) $ enemies p
squareIsDefended p sq = any (threatens sq) $ friendlies p

enemyKingIsSafe p = not $ squareIsDefended p enemyKingSq
    where enemyKingSq = enemy (Officer King) p
