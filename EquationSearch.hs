module EquationSearch(modify_and_keep_best) where

import CAS
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Types


timeout_seconds :: Int = 10
threads :: Integer = 20
reset_prob_percent :: Integer = 10
penalty_function = equations_num_nodes

modify_and_keep_best :: TVar [Equation] -> IO ()
modify_and_keep_best tvar = do
  equations <- atomically $ readTVar tvar
  best_penalty <- atomically $ return $ penalty_function equations
  tids <- mapM (\x -> forkIO (iterate_modify equations best_penalty)) [1..threads]
  threadDelay $ timeout_seconds * (10 ^ 6)
  _ <- mapM (killThread) tids
  pure ()
  where 
    iterate_modify :: [Equation] -> Integer -> IO ()
    iterate_modify equations best_penalty = do
      modified_equations <- modify_equations_randomly equations
      new_penalty <- atomically $ return $ penalty_function modified_equations
      random_number <- randomRIO (1, 100)
      if (new_penalty < best_penalty)
        then do
          atomically $ writeTVar tvar modified_equations
          iterate_modify modified_equations new_penalty
        else
          if (random_number <= reset_prob_percent)
            then do -- restart at known best set of equations
              equations <- atomically $ readTVar tvar
              iterate_modify equations best_penalty
            else do
              iterate_modify modified_equations best_penalty

equations_num_nodes :: [Equation] -> Integer
equations_num_nodes equations = sum $ map equation_num_nodes equations

equation_num_nodes :: Equation -> Integer
equation_num_nodes (Equal expression1 expression2) = (expression_num_nodes expression1) + (expression_num_nodes expression2)

expression_num_nodes :: Expression -> Integer
expression_num_nodes (Function _ subexpressions) = 1 + sum (map expression_num_nodes subexpressions)
expression_num_nodes _ = 1
