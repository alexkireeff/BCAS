module CAS1 where

import System.Random (randomRIO)

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
-- TODO need a diff function for this when we get multiplication and division
equation_function_inverse (Equal expression1 expression2) = Equal (Function "-" [expression1, expression2]) (Integer 0)



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



-- Define the expression rules
expression_rules :: [Expression -> Expression]
expression_rules = [expression_function_definitions, expression_commutive, expression_associative, expression_duals]



apply_random_expression_random_rule :: Expression -> IO Expression
apply_random_expression_random_rule (Function function subexpressions) = do
  let num_nodes = expression_nodes(Function function subexpressions)
  node <- randomRIO (0, num_nodes - 1)
  if (0 == node)
  then apply_expression_random_rule (Function function subexpressions)
  else do
    let subexpr = get_subexpression (node - 1) subexpressions
    new_subexpr <- apply_random_expression_random_rule subexpr
    return $ Function function (replace_subexpression (node - 1) new_subexpr subexpressions)
apply_random_expression_random_rule expression = apply_expression_random_rule expression



apply_expression_random_rule :: Expression -> IO Expression
apply_expression_random_rule expr = do
  rule <- randomRIO (0, length expression_rules - 1)
  return $ (expression_rules !! rule) expr



get_subexpression :: Int -> [Expression] -> Expression
get_subexpression _ [] = error "Index out of range"
get_subexpression 0 (x:_) = x
get_subexpression n (x:xs) =
  let
    nodes = expression_nodes x
    next_index = n - nodes
  in
    if (0 > next_index)
    then x
    else get_subexpression next_index xs



replace_subexpression :: Int -> Expression -> [Expression] -> [Expression]
replace_subexpression _ _ [] = error "Index out of range"
replace_subexpression 0 new_subexpr (_:xs) = new_subexpr:xs
replace_subexpression n new_subexpr (x:xs) =
  let
    nodes = expression_nodes x
    next_index = n - nodes
  in
  if (0 > next_index)
    then new_subexpr:xs
    else x : replace_subexpression (n - (expression_nodes x)) new_subexpr xs



expression_nodes :: Expression -> Int
expression_nodes (Function _ subexpressions) = 1 + sum (map expression_nodes subexpressions)
expression_nodes _ = 1
