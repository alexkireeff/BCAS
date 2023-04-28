import Types

-- Expression Rules
expression_function_definitions :: Expression -> Expression
-- addition
expression_function_definitions (Function "+" [Integer int1, Integer int2]) = Integer $ int1 + int2
expression_function_definitions (Function "+" [expression1, Integer 0]) = expression1
-- subtraction
expression_function_definitions (Function "-" [Integer int1, Integer int2]) = Integer $ int1 - int2
expression_function_definitions (Function "-" [expression1, Integer 0]) = expression1
expression_function_definitions (Function "-" [expression1, expression2]) =
    if (expression1 == expression2)
    then Integer 0
    else Function "-" [expression1, expression2]
expression_function_definitions expression = expression



-- commutivity
expression_commutive =
    let
        commutative_functions = ["+"]

        expression_commutive :: Expression -> Expression
        expression_commutive (Function function1 [expression1, expression2]) =
            if (elem function1 commutative_functions)
            then Function function1 [expression2, expression1]
            else Function function1 [expression1, expression2]
        expression_commutive expression = expression
    in
        expression_commutive



-- associativity
expression_associative = 
    let
        associative_functions = ["+"]
        expression_associative :: Expression -> Expression
        expression_associative (Function function1 [expression1, Function function2 [expression2, expression3]]) = 
            if (and [function1 == function2, elem function1 associative_functions])
            then Function function1 [Function function1 [expression1, expression2], expression3]
            else Function function1 [expression1, Function function2 [expression2, expression3]]
        expression_associative (Function function1 [Function function2 [expression1, expression2], expression3]) = 
            if (and [function1 == function2, elem function1 associative_functions])
            then Function function1 [expression1, Function function1 [expression2, expression3]]
            else Function function1 [Function function2 [expression1, expression2], expression3]
        expression_associative expression = expression
    in
        expression_associative



-- add <-> sub and mul <-> div
expression_duals :: Expression -> Expression
expression_duals (Function "-" [expression1, expression2]) = Function "+" [expression1, Function "*" [Integer (-1), expression2]]
expression_duals (Function "+" [expression1, Function "*" [Integer (-1), expression2]]) = Function "-" [expression1, expression2]
expression_duals expression = expression



-- Equation Rules
-- rename from inverse idk to what tho
equation_function_inverse :: Equation -> Equation
equation_function_inverse (Equal expression1 (Function "+" [expression2, expression3])) = Equal (Function "-" [expression1, expression3]) expression2
equation_function_inverse (Equal expression1 (Function "-" [expression2, expression3])) = Equal (Function "+" [expression1, expression3]) expression2
equation_function_inverse (Equal expression1 expression2) = Equal (Function "-" [expression1, expression2]) (Integer 0) -- TODO need a diff function for this when we get multiplication and division



equation_commutive :: Equation -> Equation
equation_commutive (Equal expression1 expression2) = Equal expression2 expression1



equations_substitution = 
    let
        helper_expression_substitution :: (Expression -> Expression) -> Expression -> Expression
        helper_expression_substitution (substitute) (expression) = 
            case (expression == substitute expression, expression) of
                (False, _) -> substitute expression
                (True, Function function1 expression_list) -> Function function1 (map (substitute) expression_list)
                (_, _) -> expression

        equations_substitution :: Equation -> Equation -> Equation
        equations_substitution (Equal expression1 expression2) (Equal expression3 expression4) =
            let
                substitute input_expression = if input_expression == expression1 then expression2 else input_expression
            in
                Equal (substitute expression3) (substitute expression4)
    in
        equations_substitution


-- Modify
-- Equations, choose 2 (without replacement)
-- if substitution, call substitute
-- else choose a random subexpression in equation 1
-- choose a random subexpression
-- apply a random function
