

data Square = Square
data Position = Position

data Move = Move
data MoveError = MoveError

move :: Move -> Position -> Either MoveError Position
move mv p
