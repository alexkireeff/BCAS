module Parser (parse) where

import Types



-- prefix notation parser
parse :: String -> [Expression]
parse input = helper_parse (reverse $ words input) []


helper_parse :: [String] -> [Expression] -> [Expression]
helper_parse [] expr =
    if (1 == length expr)
    then expr
    else error "Parse Error: Too many expressions"
helper_parse ("i" : rest) xs = helper_parse rest (Imaginary : xs)
helper_parse (pat : rest) xs =
    if (is_function pat)
    then
        case (xs) of
        (x:y:xs) -> helper_parse rest (Function pat [x, y] : xs)
        (_) -> error "Parse Error: Incorrect function syntax"
    else
        if (is_int pat)
        then helper_parse rest (Integer (read pat :: Int) : xs)
        else helper_parse rest (Variable pat : xs)


is_int :: String -> Bool
is_int [] = True
is_int (x : xs) = and [elem x "1234567890", is_int xs]


is_function :: String -> Bool
is_function x = elem x function_list
    where
        function_list = ["+", "-"]
