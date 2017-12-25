data State = Normal | Garbage | IgnoreNextChar deriving Show
data Group = Group [Group]

cleanStream = cleanStream' Normal

cleanStream' _ [] = []
cleanStream' Normal (x:xs)
    | x == '<'      = cleanStream' Garbage xs
    | x == ','      = cleanStream' Normal xs -- we don't need commas
    | otherwise     = x : (cleanStream' Normal xs)

cleanStream' Garbage (x:xs)
    | x == '>'      = cleanStream' Normal xs
    | x == '!'      = cleanStream' IgnoreNextChar xs
    | otherwise     = cleanStream' Garbage xs

cleanStream' IgnoreNextChar (x:xs) = cleanStream' Garbage xs

computeSum = snd . foldl computeSum' (1, 0)

computeSum' (l, v) '{' = (l + 1, v + l)
computeSum' (l, v) '}' = (l - 1, v)

solveA inputFile = do
    input <- readFile inputFile
    return $ computeSum $ cleanStream input

-- -- --

countGarbage = countGarbage' Normal

countGarbage' _ [] = 0
countGarbage' Normal (x:xs)
    | x == '<'      = countGarbage' Garbage xs
    | otherwise     = countGarbage' Normal xs

countGarbage' Garbage (x:xs)
    | x == '>'      = countGarbage' Normal xs
    | x == '!'      = countGarbage' IgnoreNextChar xs
    | otherwise     = 1 + countGarbage' Garbage xs

countGarbage' IgnoreNextChar (x:xs) = countGarbage' Garbage xs

solveB inputFile = do
    input <- readFile inputFile
    return $ countGarbage input

-- -- -- another solution for B

countGarbage2' (Normal,         v) '<'  = (Garbage,        v)
countGarbage2' (Normal,         v)  _   = (Normal,         v)
countGarbage2' (Garbage,        v) '>'  = (Normal,         v)
countGarbage2' (Garbage,        v) '!'  = (IgnoreNextChar, v)
countGarbage2' (Garbage,        v)  _   = (Garbage,        v + 1)
countGarbage2' (IgnoreNextChar, v)  _   = (Garbage,        v)

countGarbage2 = snd . foldl countGarbage2' (Normal, 0)

solveB2 inputFile = do
    input <- readFile inputFile
    return $ countGarbage2 input
