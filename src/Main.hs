import CAS
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import EquationSearch
import Parser
import Types

main :: IO ()
main = do
  putStrLn "Basic Computer Algebra System"
  equations <- interactWithEquations []
  putStrLn "Final Equations:"
  displayEquations equations

interactWithEquations :: [Equation] -> IO [Equation]
interactWithEquations equations = do
  putStrLn "Enter a command (add, remove, display, simplify, exit):"
  input <- getLine
  case input of
    "add" -> do
      putStrLn "Enter the left-hand side of the equation:"
      lhsInput <- getLine
      putStrLn "Enter the right-hand side of the equation:"
      rhsInput <- getLine
      case parseEquation lhsInput rhsInput of
        Just equation -> do
          putStrLn "Equation added."
          interactWithEquations (equations ++ [equation])
        Nothing -> do
          putStrLn "Invalid equation. Please try again."
          interactWithEquations equations
    "remove" -> do
      putStrLn "Enter the index of the equation to remove:"
      indexInput <- getLine
      case parseIndex indexInput of
        Just index ->
          case removeEquation index equations of
            Just newEquations -> do
              putStrLn "Equation removed."
              interactWithEquations newEquations
            Nothing -> do
              putStrLn "Invalid index. Please try again."
              interactWithEquations equations
        Nothing -> do
          putStrLn "Invalid index. Please try again."
          interactWithEquations equations
    "display" -> do
      putStrLn "Equations:"
      displayEquations equations
      interactWithEquations equations
    "simplify" -> do
      putStrLn "Simplifying equations..."
      tEquations <- newTVarIO equations
      modify_and_keep_best tEquations
      simplifiedEquations <- readTVarIO tEquations
      putStrLn "Equations simplified."
      displayEquations simplifiedEquations
      interactWithEquations simplifiedEquations
    "exit" -> return equations
    _ -> do
      putStrLn "Invalid command. Please try again."
      interactWithEquations equations

parseEquation :: String -> String -> Maybe Equation
parseEquation lhs rhs = do
  lhsExprs <- parse lhs
  rhsExprs <- parse rhs
  case (lhsExprs, rhsExprs) of
    ([lhsExpr], [rhsExpr]) -> Just (Equal lhsExpr rhsExpr)
    _ -> Nothing

parseIndex :: String -> Maybe Int
parseIndex input = case reads input of
  [(index, "")] -> Just index
  _ -> Nothing

removeEquation :: Int -> [a] -> Maybe [a]
removeEquation index xs
  | index >= 0 && index < length xs = Just (removeAtIndex index xs)
  | otherwise = Nothing

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex _ [] = []
removeAtIndex 0 (_ : xs) = xs
removeAtIndex index (x : xs) = x : removeAtIndex (index - 1) xs

displayEquations :: [Equation] -> IO ()
displayEquations equations =
  mapM_ printIndexedEquation (zip [0 ..] equations)

printIndexedEquation :: (Int, Equation) -> IO ()
printIndexedEquation (index, equation) = do
  putStrLn $ "[" ++ show index ++ "] " ++ show equation
