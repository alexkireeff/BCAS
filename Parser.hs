module Parser where

import Types



is_int :: String -> Bool
is_int [] = True
is_int (x : xs) = and [elem x "1234567890", is_int xs]


parse_wrapper :: String -> [Expression]
parse_wrapper input = parse (reverse $ words input) []

parse :: [String] -> [Expression] -> [Expression]
parse [] expr = expr
parse ("+" : rest) (x:y:xs) = parse rest ((Function "+" [x, y]) : xs)
parse ("-" : rest) (x:y:xs) = parse rest ((Function "-" [x, y]) : xs)
parse ("i" : rest) xs = parse rest (Imaginary : xs)
parse (pat : rest) xs =
    if (is_int pat)
    then parse rest (Integer (read pat :: Int) : xs)
    else parse rest (Variable pat : xs)

