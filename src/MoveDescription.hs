{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module MoveDescription where
import Prelude ()
import Data.Eq
import Data.Maybe
import Text.Show
import Control.Lens

import Square
import Piece
import MoveType

data Description src = Description { _source :: src, _destination :: Square, _moveType :: MoveType }

deriving instance Show src => Show (Description src)
deriving instance Eq src => Eq (Description src)

makeLenses ''Description
