{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Move where
import Data.Eq
import Data.Maybe
import Text.Show
import Control.Lens

import Square
import Piece
import MoveType
import MoveDescription as Description
import MoveQualifier as Qualifier


data Move src = 
    PawnMove { _description :: Description src, _promotion :: Maybe OfficerType } | 
    OfficerMove { _description :: Description src, _officerType :: OfficerType }

makeLenses ''Move


-- Make Move an instance of Qualifier when applicable.
-- This allows us to filter out non-qualifying moves by
-- mv `qualifier` src
instance (Qualifier a) => Qualifier (Move a) where
    qualifies mv = qualifies (mv ^. Move.source)


deriving instance Show src => Show (Move src)
deriving instance Eq src => Eq (Move src)


source :: Lens (Move a) (Move b) a b
source = description . Description.source


destination :: Simple Lens (Move src) Square
destination = description . Description.destination


moveType :: Simple Lens (Move src) MoveType
moveType = description . Description.moveType


pieceType :: Getter (Move src) PieceType
pieceType = to $ \ mv ->
    case mv of
        PawnMove {} -> Pawn
        OfficerMove {} -> Officer (_officerType mv)


isCapture mv = Captures == mv ^. Move.moveType


isPawnMove (PawnMove _ _) = True
isPawnMove _ = False
