module MoveType (
    MoveType(..),
    movetypes,
    ) where

data MoveType = Takes | Moves deriving (Show, Eq)

movetypes = [Takes, Moves]
