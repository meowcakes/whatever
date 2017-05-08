module Whatever.Braces where

import Data.Maybe
import Control.Monad

matchedStack :: String -> Char -> Maybe String
matchedStack (top:stack) next | top == next = Just stack
matchedStack _ _ = Nothing

popNext :: String -> Char -> Maybe String
popNext stack next = case next of
  '(' -> Just $ ')':stack
  '[' -> Just $ ']':stack
  '{' -> Just $ '}':stack
  _ -> matchedStack stack next

isBalanced :: Maybe String -> String
isBalanced (Just []) = "YES"
isBalanced _ = "NO"

braces :: [String] -> [String]
braces lines = map (isBalanced . foldM popNext []) lines
