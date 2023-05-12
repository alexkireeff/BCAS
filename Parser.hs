module Parser (parse) where

import Types

import Data.Maybe

-- prefix notation parser
parse :: String -> Maybe [Expression]
parse input = helper_parse (reverse (words input)) $ Just []

helper_parse :: [String] -> Maybe [Expression] -> Maybe [Expression]
helper_parse _ Nothing = Nothing
helper_parse [] (Just expr)
  | (1 == length expr) = Just expr
  | otherwise = Nothing
helper_parse ("i" : rest) (Just xs) = helper_parse rest $ Just $ Imaginary : xs
helper_parse (pat : rest) (Just xs)
  | (is_function pat),
    (x : y : xs) <- xs =
      helper_parse rest $ Just $ Function pat [x, y] : xs
  | (is_function pat) = Nothing
  | (is_int pat) = helper_parse rest $ Just $ Integer (read pat :: Int) : xs
  | otherwise = helper_parse rest $ Just $ Variable pat : xs

is_int :: String -> Bool
is_int [] = True
is_int (x : xs) = and [elem x "1234567890", is_int xs]

is_function :: String -> Bool
is_function x = elem x function_list
  where
    function_list = ["+", "-"]
