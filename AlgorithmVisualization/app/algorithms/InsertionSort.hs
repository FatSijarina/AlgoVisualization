module Algorithms.InsertionSort (insertionSortSteps) where

import Common (SortStep(..))

insertionSortSteps :: (Ord a) => [a] -> [SortStep a]
insertionSortSteps xs = go xs [] [] 
  where
    go [] sorted acc = reverse (SortStep sorted [] (Nothing, Nothing) : acc)
    go (x:xs) sorted acc =
      let (newSorted, activeIndices, i, j) = insertWithSteps x sorted 0
      in go xs newSorted (SortStep (sorted ++ xs) activeIndices (Just i, Just j) : acc)

    insertWithSteps x [] _ = ([x], [0], 0, -1)
    insertWithSteps x (y:ys) idx
      | x <= y    = (x : y : ys, [idx], idx, idx) 
      | otherwise = let (inserted, activeIndices, newIdx, compareIdx) = insertWithSteps x ys (idx + 1)
                    in (y : inserted, activeIndices ++ [compareIdx], newIdx, compareIdx)
