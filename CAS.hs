data Expression =
    Variable String | Integer Int | Imaginary | Function String [Expression]
    deriving (Show, Eq)

data Equation =
    Equal Expression Expression
    deriving (Show, Eq)



-- TODO we have stuff that goes in one direction, but it may also have to go in the reverse direction sometimes...
-- basically we have to check for the dual of a lot of stuff...



-- Expression Rules
expression_function_definitions :: Expression -> Maybe Expression

-- addition
expression_function_definitions (Function "+" [Integer int1, Integer int2]) = Just $ Integer $ int1 + int2
expression_function_definitions (Function "+" [Integer 0, expression2]) = Just $ expression2
expression_function_definitions (Function "+" [expression1, expression2]) =
    if (expression1 == expression2)
    then Just $ Function "*" [Integer 2, expression1] -- TODO not powers of 2
    else Nothing

-- subtraction
expression_function_definitions (Function "-" [Integer int1, Integer int2]) = Just $ Integer $ int1 - int2
expression_function_definitions (Function "-" [expression1, Integer 0]) = Just $ expression1
expression_function_definitions (Function "-" [expression1, expression2]) =
    if (expression1 == expression2)
    then Just $ Integer 0
    else Nothing

-- multiplication
expression_function_definitions (Function "*" [Integer int1, Integer int2]) = Just $ Integer $ int1 * int2
expression_function_definitions (Function "*" [Integer x, expression2]) = 
    case (x) of
    0 -> Just $ Integer 0
    1 -> Just $ expression2
    n -> Just $ Function "+" [expression2, Function "*" [Integer (n-1), expression2]] -- TODO negative numbers
expression_function_definitions (Function "*" [expression1, expression2]) = 
    Just $ Function "+" [expression2, Function "*" [Funciton "-" [expression1, Integer 1], expression2])] -- TODO negative numbers

-- division
expression_function_definitions (Function "/" [Integer int1, Integer int2]) =
    let
        divModMaybe :: Integral a => a -> a -> Maybe(a, a)
        divModMaybe _ 0 = Nothing
        divModMaybe n d = Just $ divMod n d
    in
        case (divModMaybe int1 int2) of
        Just (q, 0) -> Just $ Integer q
        Nothing -> Nothing
expression_function_definitions (Function "/" [expression1, Integer (-1)]) = Just $ (Function "*" [Integer (-1), expression1])
expression_function_definitions (Function "/" [Integer 0, expression2]) = Just $ Integer 0
expression_function_definitions (Function "/" [expression1, Integer 1]) = Just $ expression1
expression_function_definitions (Function "/" [expression1, expression2]) =
    if (expression1 == expression2)
    then Just $ Integer 1
    else Nothing

expression_function_definitions _ = Nothing


















expression_commutive_with_params :: [String] -> Expression -> Maybe Expression
expression_commutive_with_params commutative_functions (Function function1 [expression1, expression2]) =
    if (elem function1 commutative_functions)
    then Just $ Function function1 [expression2, expression1]
    else Nothing
expression_commutive_with_params _ _ = Nothing

expression_commutive = expression_commutive_with_params ["+", "*"]



expression_associative_with_params :: [String] -> Expression -> Maybe Expression
expression_associative_with_params associative_functions (Function function1 [expression1, (Function function2 [expression2, expression3])]) =
    if (and [function1 == function2, elem function1 associative_functions])
    then Just $ Function function1 [(Function function2 [expression1, expression2]), expression3]
    else Nothing
expression_associative_with_params _ _ = Nothing

expression_associative = expression_associative_with_params ["+", "*"]



expression_distributive_with_params :: [(String, String)] -> Expression -> Maybe Expression
expression_distributive_with_params distributive_pairs (Function function1 [(Function function2 [expression1, expression2]), (Function function3 [expression3, expression4])]) =
    if (and [expression2 == expression4, function2 == function3, elem (function2, function1) distributive_pairs])
    then Just $ Function function2 [(Function function1 [expression1, expression3]), expression4]
    else Nothing
expression_distributive_with_params distributive_pairs (Function function1 [(Function function2 [expression1, expression2]), expression3]) =
    if (elem (function1, function2) distributive_pairs)
    then Just $ Function function2 [(Function function1 [expression1, expression3]), (Function function1 [expression2, expression3])]
    else Nothing
expression_distributive_with_params _ _ = Nothing

expression_distributive = expression_distributive_with_params [("*", "+"), ("*", "-"), ("/", "+"), ("/", "-")]



expression_function_equivalences :: Expression -> Maybe Expression
-- a - (0 - b) = a + b
expression_function_equivalences (Function "-" [expression1, Function "-" [Integer (0), expression2]]) = Just $ Function "+" [expression1, expression2]
expression_function_equivalences (Function "+" [expression1, expression2]) = Just $ Function "-" [expression1, Function "-" [Integer (0), expression2]]
-- a / (1 / b) = a * b
expression_function_equivalences (Function "/" [expression1, Function "/" [Integer 1, expression2]]) = Just $ Function "*" [expression1, expression2]
expression_function_equivalences (Function "*" [expression1, expression2]) = Just $ Function "/" [expression1, Function "/" [Integer 1, expression2]]

-- a + (-1 * b) = a - b
expression_function_equivalences (Function "+" [expression1, Function "*" [Integer (-1), expression2]]) = Just $ Function "-" [expression1, expression2]
expression_function_equivalences (Function "-" [expression1, expression2]) = Just $ Function "+" [expression1, Function "*" [Integer (-1), expression2]]

-- TODO am I missing a dual here?

expression_function_equivalences _ = Nothing












-- Equation Rules
-- rename from inverse idk to what tho
equation_function_inverse :: Equation -> Maybe Equation
equation_function_inverse (Equal expression1 (Function "+" [expression2, expression3])) = Just (Equal (Function "-" [expression1, expression3]) expression2)
equation_function_inverse (Equal expression1 (Function "-" [expression2, expression3])) = Just (Equal (Function "+" [expression1, expression3]) expression2)
equation_function_inverse (Equal expression1 (Function "*" [expression2, expression3])) = Just (Equal (Function "/" [expression1, expression3]) expression2)
equation_function_inverse (Equal expression1 (Function "/" [expression2, expression3])) = Just (Equal (Function "*" [expression1, expression3]) expression2)
equation_function_inverse _ = Nothing

-- equation_substitution :: Equation -> Equation -> Maybe Equation
-- equation_substitution



-- Equations Rules
-- equations_substitution :: Equation -> Equation -> Equation -- TODO
-- equations_substitution (Equal expression1 expression2) (Equal expression1 expression2) = (Equal expression1 expression2)

