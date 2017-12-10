input = 1

-- 1st part

solveA input = abs x + abs y
    where 
        (x, y) = solveARecRight input 0 0

solveARecUp input x y
    | input == 1    = (x, y)
    | x == y        = solveARecLeft input x y
    | otherwise     = solveARecUp (input - 1) x (y + 1)

solveARecLeft input x y
    | input == 1    = (x, y)
    | -x == y       = solveARecDown input x y
    | otherwise      = solveARecLeft (input - 1) (x - 1) y

solveARecDown input x y
    | input == 1    = (x, y)
    | -x == -y      = solveARecRight input x y
    | otherwise     = solveARecDown (input -1) x (y - 1)

solveARecRight input x y
    | input == 1    = (x, y)
    | x - 1 == -y   = solveARecUp input x y
    | otherwise     = solveARecRight (input - 1) (x + 1) y

-- 2nd part

type Position = (Integer, Integer)
data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight deriving (Eq, Show)
data MoveInfo = MoveInfo Position Direction deriving (Eq, Show)

adjacentSquares (x, y) = [ 
    (x - 1, y - 1),
    (x - 1, y    ),
    (x - 1, y + 1),
    (x,     y - 1),
    (x,     y + 1),
    (x + 1, y - 1),
    (x + 1, y    ),
    (x + 1, y + 1)]


nextMove (MoveInfo (x, y) DirectionUp)
    | x == y        = MoveInfo (x - 1,  y    )      DirectionLeft
    | otherwise     = MoveInfo (x,      y + 1)  DirectionUp

nextMove (MoveInfo (x, y) DirectionLeft)
    | -x == y       = MoveInfo (x,      y - 1)  DirectionDown
    | otherwise     = MoveInfo (x - 1,  y    )  DirectionLeft

nextMove (MoveInfo (x, y) DirectionDown)
    | -x == -y      = MoveInfo (x + 1,  y    )  DirectionRight
    | otherwise     = MoveInfo (x,      y - 1)  DirectionDown

nextMove (MoveInfo (x, y) DirectionRight)
    | x -1  == -y   = MoveInfo (x,      y + 1)  DirectionUp
    | otherwise     = MoveInfo (x + 1,  y    )  DirectionRight


valuesForSquares squares values moveInfo
    | null values          = []
    | elem (x, y) squares   = firstValue : otherValues
    | otherwise             = otherValues
    where
        (firstValue: nextValues) = values
        MoveInfo (x, y) direction = moveInfo
        otherValues = valuesForSquares squares nextValues (nextMove moveInfo)

valueInSquare position currentValues = sum $ valuesForSquares (adjacentSquares position) currentValues (MoveInfo (0,0) DirectionRight)

solveB input = solveBB input [1] (MoveInfo (1, 0) DirectionRight)

solveBB input acc moveInfo
    | newValue > input  = newValue
    | otherwise         = solveBB input (newValue:acc) (nextMove moveInfo)
    where
        newValue = valueInSquare (x, y) (reverse acc)
        MoveInfo (x, y) _ = moveInfo