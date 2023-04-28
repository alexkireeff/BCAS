data Expression =
    Variable String | Integer Int | Imaginary | Function String [Expression]
    deriving (Show, Eq)

data Equation =
    Equal Expression Expression
    deriving (Show, Eq)



-- Expression Rules
expression_function_definitions :: Expression -> Maybe Expression
-- addition
expression_function_definitions (Function "+" [Integer int1, Integer int2]) = Just $ Integer $ int1 + int2
expression_function_definitions (Function "+" [expression1, Integer 0]) = Just $ expression1
-- subtraction
expression_function_definitions (Function "-" [Integer int1, Integer int2]) = Just $ Integer $ int1 - int2
expression_function_definitions (Function "-" [expression1, Integer 0]) = Just $ expression1
expression_function_definitions (Function "-" [expression1, expression2]) =
    if (expression1 == expression2)
    then Just $ Integer 0
    else Nothing
expression_function_definitions _ = Nothing



-- commutivity
expression_commutive =
    let
        commutative_functions = ["+"]

        expression_commutive :: Expression -> Maybe Expression
        expression_commutive (Function function1 [expression1, expression2]) =
            if (elem function1 commutative_functions)
            then Just $ Function function1 [expression2, expression1]
            else Nothing
        expression_commutive _ = Nothing
    in
        expression_commutive



-- associativity
expression_associative = 
    let
        associative_functions = ["+"]
        expression_associative :: Expression -> Maybe Expression
        expression_associative (Function function1 [expression1, Function function2 [expression2, expression3]]) = 
            if (and [function1 == function2, elem function1 associative_functions])
            then Just $ Function function1 [Function function1 [expression1, expression2], expression3]
            else Nothing
        expression_associative (Function function1 [Function function2 [expression1, expression2], expression3]) = 
            if (and [function1 == function2, elem function1 associative_functions])
            then Just $ Function function1 [expression1, Function function1 [expression2, expression3]]
            else Nothing
        expression_associative _ = Nothing
    in
        expression_associative



-- add <-> sub and mul <-> div
expression_duals :: Expression -> Maybe Expression
expression_duals (Function "-" [expression1, expression2]) = Just $ Function "+" [expression1, Function "*" [Integer (-1), expression2]]
expression_duals (Function "+" [expression1, Function "*" [Integer (-1), expression2]]) = Just $ Function "-" [expression1, expression2]
expression_duals _ = Nothing



-- Equation Rules
-- rename from inverse idk to what tho
equation_function_inverse :: Equation -> Maybe Equation
equation_function_inverse (Equal expression1 (Function "+" [expression2, expression3])) = Just $ Equal (Function "-" [expression1, expression3]) expression2
equation_function_inverse (Equal expression1 (Function "-" [expression2, expression3])) = Just $ Equal (Function "+" [expression1, expression3]) expression2
equation_function_inverse (Equal expression1 expression2) = Just $ Equal (Function "-" [expression1, expression2]) (Integer 0)



equation_commutive :: Equation -> Maybe Equation
equation_commutive (Equal expression1 expression2) = Just $ Equal expression2 expression1



equation_substitution = 
    let
        helper_expression_substitution :: (Expression -> Expression) -> Expression -> Expression
        helper_expression_substitution (substitute) (expression) = 
            case (expression == substitute expression, expression) of
                (False, _) -> substitute expression
                (True, Function function1 expression_list) -> Function function1 (map (substitute) expression_list)
                (_, _) -> expression

        equation_substitution :: Equation -> Equation -> Maybe Equation
        equation_substitution (Equal expression1 expression2) (Equal expression3 expression4) =
            let
                substitute input_expression = if input_expression == expression1 then expression2 else input_expression
            in
                if and [expression3 == substitute expression3, expression4 == substitute expression4]
                then Nothing
                else Just $ Equal (substitute expression3) (substitute expression4)
    in
        equation_substitution



-- Equations Rules
-- equations_substitution :: Equation -> Equation -> Equation -- TODO
-- equations_substitution (Equal expression1 expression2) (Equal expression1 expression2) = (Equal expression1 expression2)

