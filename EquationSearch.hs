module EquationSearch(modify_and_keep_best) where

import CAS
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Types


reset_every_n_iterations = 100
penalty_function = equations_num_nodes

modify_and_keep_best :: TVar [Equation] -> Integer -> IO ()
modify_and_keep_best tvar iterations_remaining = do
  equations <- atomically $ readTVar tvar
  best_penalty <- atomically $ return $ penalty_function equations
  iterate_modify equations best_penalty iterations_remaining
  where
    iterate_modify :: [Equation] -> Integer -> Integer -> IO ()
    iterate_modify _ _ 0 = return ()
    iterate_modify equations best_penalty iterations_remaining = do
      modified_equations <- modify_equations_randomly equations
      new_penalty <- atomically $ return $ penalty_function modified_equations
      if (new_penalty < best_penalty)
        then do
          atomically $ writeTVar tvar modified_equations
          iterate_modify modified_equations new_penalty (iterations_remaining - 1)
        else
          if (0 == rem iterations_remaining reset_every_n_iterations) -- restart at known best set of equations
            then do
              equations <- atomically $ readTVar tvar
              iterate_modify equations best_penalty (iterations_remaining - 1)
            else do
              iterate_modify modified_equations best_penalty (iterations_remaining - 1)

equations_num_nodes :: [Equation] -> Integer
equations_num_nodes equations = sum $ map equation_num_nodes equations

equation_num_nodes :: Equation -> Integer
equation_num_nodes (Equal expression1 expression2) = (expression_num_nodes expression1) + (expression_num_nodes expression2)

expression_num_nodes :: Expression -> Integer
expression_num_nodes (Function _ subexpressions) = 1 + sum (map expression_num_nodes subexpressions)
expression_num_nodes _ = 1
