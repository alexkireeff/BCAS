data Expression =
    Variable String | Integer Int | Imaginary | Function String [Expression]
    deriving (Show, Eq)

data Equation =
    Equal Expression Expression
    deriving (Show, Eq)



function_definitions :: Expression -> Maybe Expression

-- addition
function_definitions (Function "+" [Integer int1, Integer int2]) = Just $ Integer $ int1 + int2
function_definitions (Function "+" [Integer 0, expression2]) = Just $ expression2
function_definitions (Function "+" [expression1, expression2]) = 
    if (expression1 == expression2)
    then Just $ Function "*" [Integer 2, expression1]
    else Nothing

-- subtraction
function_definitions (Function "-" [Integer int1, Integer int2]) = Just $ Integer $ int1 - int2
function_definitions (Function "-" [Integer 0, expression2]) = Just $ expression2
function_definitions (Function "-" [expression1, expression2]) =
    if (expression1 == expression2)
    then Just $ Integer 0
    else Nothing

-- multiplication
function_definitions (Function "*" [Integer int1, Integer int2]) = Just $ Integer $ int1 * int2
function_definitions (Function "*" [Integer 0, expression2]) = Just $ Integer 0
-- squared
-- function_definitions (Function "*" [expression1, expression2]) =
--   if (expression1 == expression2)
--   then Just $ Function "pow" [2, expression1]
--   else Nothing

-- division
function_definitions (Function "/" [Integer int1, Integer int2]) = 
    let
        divModMaybe :: Integral a => a -> a -> Maybe(a, a)
        divModMaybe _ 0 = Nothing
        divModMaybe n d = Just $ divMod n d
    in
        case (divModMaybe int1 int2) of
        Just (q, 0) -> Just $ Integer q
        Nothing -> Nothing
function_definitions (Function "/" [Integer 0, expression2]) = Just $ Integer 0 -- TODO expression 2 can't be = 0
function_definitions (Function "/" [expression1, expression2]) = 
    if (expression1 == expression2)
    then Just $ Integer 1
    else Nothing

function_definitions _ = Nothing



commutive_with_params :: [String] -> Expression -> Maybe Expression
commutive_with_params commutative_functions (Function function1 [expression1, expression2]) =
    if (elem function1 commutative_functions)
    then Just $ Function function1 [expression2, expression1]
    else Nothing
commutive_with_params _ _ = Nothing

commutive = commutive_with_params ["+", "*"]



associative_with_params :: [String] -> Expression -> Maybe Expression
associative_with_params associative_functions (Function function1 [expression1, (Function function2 [expression2, expression3])]) =
    if (and [function1 == function2, elem function1 associative_functions])
    then Just $ Function function1 [(Function function2 [expression1, expression2]), expression3]
    else Nothing
associative_with_params _ _ = Nothing

associative = associative_with_params ["+", "*"]



distributive_with_params :: [(String, String)] -> Expression -> Maybe Expression
distributive_with_params distributive_pairs (Function function1 [(Function function2 [expression1, expression2]), (Function function3 [expression3, expression4])]) =
    if (and [expression2 == expression4, function2 == function3, elem (function2, function1) distributive_pairs])
    then Just $ Function function2 [(Function function1 [expression1, expression3]), expression4]
    else Nothing
distributive_with_params distributive_pairs (Function function1 [(Function function2 [expression1, expression2]), expression3]) =
    if (elem (function1, function2) distributive_pairs)
    then Just $ Function function2 [(Function function1 [expression1, expression3]), (Function function1 [expression2, expression3])]
    else Nothing
distributive_with_params _ _ = Nothing

distributive = distributive_with_params [("*", "+"), ("*", "-"), ("/", "+"), ("/", "-")]



-- TODO how do we deal with Equation rules nicely? -> call subs(eqn1, eqn2) which will substitute all occurences of the left side with the right
