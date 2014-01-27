module Color where


data Color = Black | White deriving (Show, Eq, Enum)


invert c = case c of
    White -> Black
    Black -> White


char c =
  case c of
    White -> 'w'
    Black -> 'b'


color :: Char -> Color
color c = if isUpper c
  then White
  else Black
