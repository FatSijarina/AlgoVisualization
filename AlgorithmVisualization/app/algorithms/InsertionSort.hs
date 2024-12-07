module Algorithms.InsertionSort (insertionSortSteps) where

import Algorithms.Common (SortStep(..))

-- Insertion sort implementation returning sorting steps
insertionSortSteps :: (Ord a) => [a] -> [SortStep a]
insertionSortSteps xs = go [] xs []
  where
    go sorted [] acc = reverse (SortStep sorted [] : acc)
    go sorted (x:xs) acc =
      let (newSorted, active) = insertWithSteps x sorted
      in go newSorted xs (SortStep (newSorted ++ xs) active : acc)

    insertWithSteps x [] = ([x], [0])
    insertWithSteps x (y:ys)
      | x <= y    = (x : y : ys, [0])
      | otherwise = let (inserted, active) = insertWithSteps x ys
                    in (y : inserted, map (+1) active)