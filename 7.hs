import Data.List

data Program = Program {
    programName :: String,
    programWeight :: Int,
    programSubprograms :: [String]
} deriving Show

toProgram text = 
    let wordList = words $ filter ((/=)',') text
    in if length wordList > 2
        then let (name : weight : _ : programs) = wordList
            in Program name (read weight :: Int) programs
        else let (name : weight : _) = wordList
            in Program name (read weight :: Int) []
        

removeDups [] = []
removeDups (x: []) = [x]
removeDups (x:y:z)
    | x == y    = removeDups (y:z)
    | otherwise = x:(removeDups (y:z))

findRootProgram programs = findProgramWithoutParent allPrograms allProgramChildren
    where
        allPrograms = sort $ map programName programs
        allProgramChildren = removeDups $ sort $ foldr1 (++) $ map programSubprograms programs


findProgramWithoutParent (program:programs) (child:children)
    | program == child      = findProgramWithoutParent programs children
    | otherwise             = program

readInputData fileName = do
    input <- readFile fileName
    return $ map toProgram $ lines input

solveA fileName = do
    programs <- readInputData fileName
    return $ findRootProgram programs



data LinkedProgram = LinkedProgram {
    lProgramName :: String,
    lProgramWeight :: Int,
    lProgramTreeWeight :: Int,
    lProgramSubprograms :: [LinkedProgram]
} deriving Show

buildLinkedProgram programs name  = LinkedProgram name weight 0 linkedSubprograms
    where
        Program _ weight subprograms = head $ filter (\p -> (programName p) == name) programs
        linkedSubprograms = map (buildLinkedProgram programs) subprograms

weightForProgram (LinkedProgram name weight treeWeight []) = (LinkedProgram name weight weight [])
weightForProgram (LinkedProgram name weight _ programs) = LinkedProgram name weight sumWeight newPrograms
    where
        newPrograms = map weightForProgram programs
        sumWeight = foldl1 (+) (weight : map lProgramTreeWeight newPrograms)

getWrongProgram (LinkedProgram _ _ _ []) = Nothing
getWrongProgram (LinkedProgram _ weight treeWeight programs) =
    if wrongSubsubprogram == Nothing then 
        if allEqual $ map lProgramTreeWeight programs then
            Nothing
        else
            Just $ map (\x -> (lProgramTreeWeight x, lProgramWeight x)) programs
    else 
        wrongSubsubprogram
    where
        wrongSubsubprogram = extractJust $ map getWrongProgram programs

allEqual [] = True
allEqual (_:[]) = True
allEqual l = (sum $ map (\x -> x - head l) l) == 0

extractJust [] = Nothing
extractJust (Nothing:xs) = extractJust xs
extractJust ((Just n):xs) = Just n


solveB fileName = do
    programs <- readInputData fileName
    let root = buildLinkedProgram programs $ findRootProgram programs
    let tree = weightForProgram root
    return $ getWrongProgram tree
