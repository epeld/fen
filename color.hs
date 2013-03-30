module Color (
    Color(..),
    ) where

data Color = Black | White deriving (Show, Eq)

invert c = case c of
    White -> Black
    Black -> White

