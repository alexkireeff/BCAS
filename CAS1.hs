module CAS1 where

import System.Random (randomRIO)

import Types

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Expression Rules

-- definitions
expression_function_definitions :: Expression -> Expression
expression_function_definitions (Function "+" [Integer int1, Integer int2]) = Integer $ int1 + int2
expression_function_definitions (Function "+" [expression1, Integer 0]) = expression1
expression_function_definitions (Function "-" [Integer int1, Integer int2]) = Integer $ int1 - int2
expression_function_definitions (Function "-" [expression1, Integer 0]) = expression1
expression_function_definitions (Function "-" [expression1, expression2]) =
    if (expression1 == expression2)
    then Integer 0
    else Function "-" [expression1, expression2]
expression_function_definitions expression = expression



-- commutivity
expression_commutive :: Expression -> Expression
expression_commutive (Function function1 [expression1, expression2]) =
    if (elem function1 commutative_functions)
    then Function function1 [expression2, expression1]
    else Function function1 [expression1, expression2]
expression_commutive expression = expression


-- TODO don't let this out of the module
commutative_functions = ["+"]



-- associativity
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


-- TODO don't let this out of the module
associative_functions = ["+"]



-- add <-> sub and mul <-> div
expression_duals :: Expression -> Expression
expression_duals (Function "-" [expression1, expression2]) = Function "+" [expression1, Function "*" [Integer (-1), expression2]]
expression_duals (Function "+" [expression1, Function "*" [Integer (-1), expression2]]) = Function "-" [expression1, expression2]
expression_duals expression = expression



-- apply an expression rule to a random expression in an equation
apply_random_equations_expression_rule :: (Expression -> Expression) -> [Equation] -> IO [Equation]
apply_random_equations_expression_rule rule (equation : equations) = do
    let eqns_num_nodes = equations_num_nodes equations
    let eqn_num_nodes = equation_num_nodes equation
    random_number <- randomRIO (1, eqn_num_nodes + eqns_num_nodes)
    new_equation <- apply_random_equation_expression_rule rule equation
    new_equations <- apply_random_equations_expression_rule rule equations
    if (random_number <= eqn_num_nodes)
    then return $ new_equation : equations
    else return $ equation : new_equations


-- apply an expression rule to a random expression in an equation
apply_random_equation_expression_rule :: (Expression -> Expression) -> Equation -> IO Equation
apply_random_equation_expression_rule rule (Equal expression1 expression2) = do
    let expression1_num_nodes = expression_num_nodes expression1
    let expression2_num_nodes = expression_num_nodes expression2
    random_number <- randomRIO (1, expression1_num_nodes + expression2_num_nodes)
    new_expression1 <- apply_random_expression_expression_rule rule expression1
    new_expression2 <- apply_random_expression_expression_rule rule expression2
    if (random_number <= expression1_num_nodes)
    then return $ Equal new_expression1 expression2
    else return $ Equal expression1 new_expression2


-- apply an expression rule to a random expression in an expression
apply_random_expression_expression_rule :: (Expression -> Expression) -> Expression -> IO Expression
apply_random_expression_expression_rule rule (Function function subexpressions) = do
    let num_nodes = expression_num_nodes(Function function subexpressions)
    random_number <- randomRIO (1, num_nodes)
    if (1 == random_number)
    then return $ rule (Function function subexpressions)
    else do
        let subexpr = get_subexpression (random_number - 2) subexpressions
        new_subexpr <- apply_random_expression_expression_rule rule subexpr
        return $ Function function (replace_subexpression (random_number - 2) new_subexpr subexpressions)
apply_random_expression_expression_rule rule expression = return $ rule expression


-- TODO don't let this out of the module
get_subexpression :: Int -> [Expression] -> Expression
get_subexpression _ [] = error "Index out of range"
get_subexpression 0 (x : _) = x
get_subexpression n (x : xs) =
  let
    num_nodes = expression_num_nodes x
    next_index = n - num_nodes
  in
    if (0 > next_index)
    then x
    else get_subexpression next_index xs


-- TODO don't let this out of the module
replace_subexpression :: Int -> Expression -> [Expression] -> [Expression]
replace_subexpression _ _ [] = error "Index out of range"
replace_subexpression 0 new_subexpr (_ : xs) = new_subexpr : xs
replace_subexpression n new_subexpr (x : xs) =
    let
        num_nodes = expression_num_nodes x
        next_index = n - num_nodes
    in
        if (0 > next_index)
        then new_subexpr:xs
        else x : replace_subexpression (n - (expression_num_nodes x)) new_subexpr xs


-- TODO don't let this out of the module
equations_num_nodes :: [Equation] -> Int
equations_num_nodes equations = sum $ map equation_num_nodes equations

-- TODO don't let this out of the module
equation_num_nodes :: Equation -> Int
equation_num_nodes (Equal expression1 expression2) = (expression_num_nodes expression1) + (expression_num_nodes expression2)


-- TODO don't let this out of the module
expression_num_nodes :: Expression -> Int
expression_num_nodes (Function _ subexpressions) = 1 + sum (map expression_num_nodes subexpressions)
expression_num_nodes _ = 1



-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equation Rules

-- rename from inverse idk to what tho
equation_function_inverse :: Equation -> Equation
equation_function_inverse (Equal expression1 (Function "+" [expression2, expression3])) = Equal (Function "-" [expression1, expression3]) expression2
equation_function_inverse (Equal expression1 (Function "-" [expression2, expression3])) = Equal (Function "+" [expression1, expression3]) expression2
-- TODO need a diff function for this when we get multiplication and division
equation_function_inverse (Equal expression1 expression2) = Equal (Function "-" [expression1, expression2]) (Integer 0)



equation_commutive :: Equation -> Equation
equation_commutive (Equal expression1 expression2) = Equal expression2 expression1



-------------------------------------------------------------------------------------------------------------------------------------------------
-- Equations Rules
equations_substitution :: Equation -> Equation -> Equation
equations_substitution (Equal expression1 expression2) (Equal expression3 expression4) =
    let
        substitute input_expression =
            if (input_expression == expression1)
                then expression2
                else input_expression
    in
        Equal (substitute expression3) (substitute expression4)


-- TODO don't let this out of the module
helper_expression_substitution :: (Expression -> Expression) -> Expression -> Expression
helper_expression_substitution (substitute) (expression) =
    case (expression == substitute expression, expression) of
        (False, _) -> substitute expression
        (True, Function function1 expression_list) -> Function function1 (map (substitute) expression_list)
        (_, _) -> expression



-- Modify
-- Equations, choose 2 (without replacement)
-- if substitution, call substitute
-- else choose a random subexpression in equation 1
-- choose a random subexpression DONE
-- apply a random function DONE
