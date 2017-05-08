module Whatever.Braces where

import Data.Maybe
import Control.Monad

matchedStack :: String -> Char -> Maybe String
matchedStack (top:stack) next | top == next = Just stack
matchedStack _ _ = Nothing

popNext :: String -> Char -> Maybe String
popNext stack next = case next of
  '(' -> match ')'
  '[' -> match ']'
  '{' -> match '}'
  _ -> matchedStack stack next
  where match = Just . (:stack)

isBalanced :: Maybe String -> String
isBalanced (Just []) = "YES"
isBalanced _ = "NO"

braces :: [String] -> [String]
braces lines = map (isBalanced . foldM popNext []) lines
