module Whatever.SeparatingStudents where

minMovesR :: Int -> Int -> [Int] -> Int
minMovesR carry score remaining
  | null remaining' = 0
  | otherwise = carry + moves + (minMovesR (moves + carry) score $ tail remaining')
  where remaining' = dropWhile (==score) remaining
        moves = length $ takeWhile (==score) remaining

minMoves :: [Int] -> Int
minMoves avg = minimum [minMovesR 0 score avg | score <- [0,1]]
