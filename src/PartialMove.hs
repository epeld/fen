{-# LANGUAGE TemplateHaskell #-}
module PartialMove where
import Control.Lens

import Move
import MoveQualifier
import MoveDescription
import Square

type PartialMove = Move (Maybe PartialSquare)


instance Qualifier PartialSquare where
    qualifies (Rank r) sq = rank sq == r
    qualifies (File f) sq = Just (file sq) == fileIndex f
    qualifies (Whole sq2) sq = sq == sq2


data PartialSquare = Rank { _rank :: Int } | File { _file :: Char } | Whole { _square :: Square } deriving Show
makeLenses ''PartialSquare
