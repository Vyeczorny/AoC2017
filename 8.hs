class Editable a where
    value :: a -> Int
    name :: a -> String
    editWithValue :: a -> Int -> a
    make :: String -> Int -> a

type RegisterName = String
type ConditionOperation = Int -> Int -> Bool
type OperationName = String

data Condition = Condition String OperationName Int deriving Show
data Operation = Operation String Int Condition deriving Show
data Register = Register String Int deriving Show

instance Eq Register where
    (==) (Register _ value1) (Register _ value2) = value1 == value2

instance Ord Register where
    compare (Register _ value1) (Register _ value2) = value1 `compare` value2

instance Editable Register where
    value (Register _ value) = value
    name (Register name _) = name
    editWithValue (Register name x) value = Register name (value + x)
    make name value = Register name value

toOperation (name : "inc" : value : "if" : name2 : conditionName : value2 : []) 
    = Operation name (read value) (Condition name2 conditionName (read value2))

toOperation (name : "dec" : value : "if" : name2 : conditionName : value2 : []) 
    = Operation name (negate $ read value) (Condition name2 conditionName (read value2))

conditionFunction ">" = (>)
conditionFunction ">=" = (>=)
conditionFunction "==" = (==)
conditionFunction "<=" = (<=)
conditionFunction "<" = (<)
conditionFunction "!=" = (/=)

toOperations = map (toOperation . words) . lines

checkCondition (Condition _ conditionName value) [] = (conditionFunction conditionName) 0 value
checkCondition condition (editable:xs)
    | (name editable) == cName   = (conditionFunction cConditionName) (value editable) cValue
    | otherwise                 = checkCondition condition xs
    where 
        (Condition cName cConditionName cValue) = condition

makeOperation registers operation = makeOperation' registers registers operation

makeOperation' [] list (Operation name value condition)
    | checkCondition condition list     = [make name value]
    | otherwise                         = []

makeOperation' (editable:xs) list operation
    | oName == eName    = (newEditable:xs)
    | otherwise         = (editable : makeOperation' xs list operation)
        where
            (Operation oName oValue oCondition) = operation
            eName = name editable
            eValue = value editable
            newEditable = if checkCondition oCondition list 
                then editWithValue editable oValue 
                else editable


makeOperations :: Editable a => [Operation] -> [a]
makeOperations = foldl makeOperation []

solveA inputName = do
    input <- readFile inputName
    return $ maximum $ (makeOperations (toOperations input) :: [Register])

-- -- -- -- 

type MaxValue = Int
data RegisterWithMax = RegisterWithMax MaxValue Register deriving Show

instance Eq RegisterWithMax where
    (==) (RegisterWithMax maxValue1 _) (RegisterWithMax maxValue2 _) = maxValue1 == maxValue2

instance Ord RegisterWithMax where
    compare (RegisterWithMax maxValue1 _) (RegisterWithMax maxValue2 _) = maxValue1 `compare` maxValue2

instance Editable RegisterWithMax where
    value (RegisterWithMax _ (Register _ value)) = value
    name (RegisterWithMax _ (Register name _)) = name
    make name value = RegisterWithMax value (Register name value)
    editWithValue (RegisterWithMax currMax (Register rName rValue)) value
        | currMax > newValue    = RegisterWithMax currMax (Register rName newValue)
        | otherwise             = RegisterWithMax newValue (Register rName newValue)
        where
            newValue = rValue + value



solveB inputName = do
    input <- readFile inputName
    return $ maximum $ (makeOperations (toOperations input) :: [RegisterWithMax])
-- -- --