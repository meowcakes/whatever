module Whatever.BudgetShopping where

import Data.Vector (Vector)
import qualified Data.Vector as V

budgetFold :: [Int] -> [Int] -> Vector Integer -> Int -> Vector Integer
budgetFold v w m n = m V.// [(n, mx)]
  where candidates = [(toInteger v') + (m V.! (n - w'))
                     | (v', w') <- zip v w,
                       w' <= n]
        mx | null candidates = 0
           | otherwise = maximum candidates

budgetShopping :: Int -> [Int] -> [Int] -> Integer
budgetShopping n v w = m V.! n
  where m = foldl (budgetFold v w) (V.replicate (n+1) 0) [1..n]
