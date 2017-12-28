import Data.Char
import Data.Bits
import Numeric

x = [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188] :: [Int]
y = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"
asciiSuffix = [17, 31, 73, 47, 23] :: [Int]

computeList skip pos [] = pos
computeList skip pos (l:ls) = computeList (skip + 1) skipped ls
    where
        reversed = (drop l pos) ++ (reverse $ take l pos)
        skipMod = (skip `mod` length pos)
        skipped = (drop skipMod reversed) ++ (take skipMod reversed)

computeShift pos lengths = foldl1 (+) (lengths ++ [0..(length lengths - 1)]) `mod` length pos

solveA pos input =
    foldl1 (*)
    $ take 2 
    $ drop (length pos - computeShift pos input) 
    $ cycle 
    $ computeList 0 pos input

-- -- --

computeSpareHash i pos lengths
    | i == 64       = pos
    | otherwise     = computeSpareHash (i+1) (computeList (i * (length lengths)) pos lengths) lengths

computeSparseShift pos lengths = 
    ((64 * foldl1 (+) lengths) + (foldl1 (+) [0..(64 * (length lengths) - 1)])) `mod` length pos

splitToChunks _ [] = []
splitToChunks n list = (take n list):(splitToChunks n $ drop n list)

alignToTwoSigns x = case length x of
    0 -> "00"
    1 -> "0" ++ x
    2 -> x

solveB lenghts =
    foldl1 (++)
    $ map (alignToTwoSigns . ($ "") . showHex . foldl1 xor)
    $ splitToChunks 16
    $ take 256
    $ drop (length pos - computeSparseShift pos intLenghts)
    $ cycle 
    $ computeSpareHash 0 pos intLenghts
    where 
        intLenghts = map ord lenghts ++ asciiSuffix
        pos = [0..255] :: [Int]