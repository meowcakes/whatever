module Whatever.BudgetShoppingGreedy where

import Data.List
import Data.Ord

budgetFold :: (Int, Int) -> (Int, Int) -> (Int, Int)
budgetFold (q, c) (remaining, total) = (remaining - (maxBundles * c), total + (maxBundles * q))
  where maxBundles = quot remaining c

sortFn :: (Int, Int) -> (Int, Int) -> Ordering
sortFn (a, b) (c, d)
  | d1 < d2 = LT
  | d1 > d2 = GT
  | otherwise = EQ
  where d1 = (fromIntegral b) / (fromIntegral a)
        d2 = (fromIntegral d) / (fromIntegral c)

budgetShopping :: Int -> [Int] -> [Int] -> Int
budgetShopping n bundleQuantities bundleCosts = snd $ foldr budgetFold (n, 0) costPerBook
  where costPerBook = sortBy sortFn $ zip bundleQuantities bundleCosts
