module Threats (enemyKingIsSafe, squareIsThreatened, squareIsDefended) where
import Control.Applicative ((<$>))

import ProjectedRange (threatens,)
import MovingPiece (friendlies, enemies)
import Piece (PieceType(..), OfficerType(King), Piece(..), )
import Position (friendlySquares, enemySquares, enemy)

squareIsThreatened p sq = not. any (threatens sq) $ enemies p
squareIsDefended p sq = not. any (threatens sq) $ friendlies p

enemyKingIsSafe p = squareIsDefended p enemyKingSq
    where enemyKingSq = enemy (Officer King) p
