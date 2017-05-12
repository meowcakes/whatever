module Whatever.BudgetShopping where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List

budgetFold :: [Int] -> [Int] -> Vector Int -> Int -> Vector Int
budgetFold v w m n = mx `seq` m V.// [(n, seq n mx)]
  where candidates = [v' + (m V.! (n - w'))
                     | (v', w') <- zip v w,
                       w' <= n]
        mx | null candidates = 0
           | otherwise = maximum candidates

budgetShopping :: Int -> [Int] -> [Int] -> Int
budgetShopping n v w = m V.! n
  where m = foldl' (budgetFold v w) (V.replicate (n+1) 0) [1..n]
