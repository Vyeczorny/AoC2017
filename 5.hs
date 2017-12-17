import Data.Array

type Position = Int
type ArrayLength = Int
data State = State (Array Int Int) ArrayLength Position

makeArray input = array (0, length input - 1) (zip [0..] input)
makeState input = State (makeArray input) (length input) 0

countMoves moveFunction counter (State array inputLength position)
    | position < -1 || position > inputLength - 1   = counter
    | otherwise                                     = countMoves moveFunction (counter + 1) newState
    where
        currentValue = array ! position
        updatedArray = array // [(position, moveFunction currentValue)]
        newState = State updatedArray inputLength (position + currentValue)


solveA = countMoves (+1) 0 . makeState
solveB = countMoves (\x -> if x >= 3 then x - 1 else x + 1) 0 . makeState


main = do
    input <- fmap (\x -> (map read $ words x) :: [Int]) $ readFile "5.data"
    putStrLn $ show $ solveB input

