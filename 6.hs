import Data.Array

type Registers = Array Int Int

test = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3] :: [Int]

cons = 16

toArray list = array (0, length list - 1) $ zip [0..] list

myMax (i, c) (i', c') = if c' > c then (i', c') else (i ,c)

findMax :: Registers -> (Int, Int)
findMax = foldl1 myMax . assocs

clearValue index registers = registers // [(index, 0)]

computeAddition :: Int -> Int -> Int -> Int
computeAddition num startingIndex index
    | index <= startingIndex && index + cons <= startingIndex + l   = k + 1
    | index > startingIndex && index <= startingIndex + l           = k + 1
    | otherwise                                                     = k
    where 
        k = num `div` cons
        l = num `mod` cons

updateRegisters (i, c) registers = updateRegisters' 0 (i, c) registers

updateRegisters' currentIndex (i, c) registers
    | currentIndex >= cons  = registers
    | otherwise             = updateRegisters' (currentIndex + 1) (i, c) (registers // newValue)
    where
        valueToAdd = computeAddition c i currentIndex
        newValue = [(currentIndex, registers!currentIndex + valueToAdd)]


countCycles cr = countCycles' [] cr 0

countCycles' sr cr counter
    | cr `elem` sr  = counter
    | otherwise     = countCycles' (cr:sr) (updateRegisters (i, c) $ clearValue i cr) (counter + 1)
    where
        (i, c) = findMax cr

indexOf elem list = indexOf' elem list 0
indexOf' elem [] counter = -1
indexOf' elem (x:xs) counter
    | x == elem = counter
    | otherwise = indexOf' elem xs (counter + 1)

countLoopLength cr = countLoopLength' [] cr 0
countLoopLength' sr cr counter
    | crIndex /= -1 = crIndex + 1
    | otherwise     = countLoopLength' (cr:sr) (updateRegisters (i, c) $ clearValue i cr) (counter + 1)
    where
        (i, c) = findMax cr
        crIndex = indexOf cr sr

solveA = countCycles . toArray

solveB = countLoopLength . toArray
