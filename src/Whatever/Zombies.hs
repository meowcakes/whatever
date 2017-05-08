module Whatever.Zombies where

import qualified Data.Set as S

zombieClusterMembership :: [Int]
                        -> S.Set Int
                        -> ([S.Set Int], [S.Set Int])
                        -> ([S.Set Int], [S.Set Int])
zombieClusterMembership relations group (member, unknown)
  | any (\x -> S.member x group) relations = (group:member, unknown)
  | otherwise = (member, group:unknown)

zombieClusterFold :: (Int, [Int]) -> [S.Set Int] -> [S.Set Int]
zombieClusterFold (self, relations) clusters = (S.unions member):unknown
  where (member, unknown) = foldr (zombieClusterMembership relations)
                            ([], [])
                            ((S.singleton self):clusters)

zombieRowToRelations :: (Int, String) -> (Int, [Int])
zombieRowToRelations (self, relations)
  = (self, map fst $ filter ((=='1') . snd) $ zip [0..] relations)

zombieCluster :: [String] -> Int
zombieCluster zombies = length $
                        foldr zombieClusterFold [] $
                        map zombieRowToRelations $
                        zip [0..] zombies
