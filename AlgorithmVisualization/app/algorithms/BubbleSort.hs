module Algorithms.BubbleSort (bubbleSortSteps) where

import Common (SortStep(..))

bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = go xs []
  where
    go lst acc
      | isSorted lst = reverse (SortStep lst [] (Nothing, Nothing) : acc)
      | otherwise =
          let (newList, active, i, j) = bubble lst 0
          in go newList (SortStep lst active (Just i, Just j) : acc)

    bubble [x] _ = ([x], [], -1, -1)
    bubble (x : y : rest) idx
      | x > y     = let (bubbled, active, i, j) = bubble (x : rest) (idx + 1)
                    in (y : bubbled, [idx, idx + 1], idx, idx + 1)
      | otherwise = let (bubbled, active, i, j) = bubble (y : rest) (idx + 1)
                    in (x : bubbled, [idx + 1], idx + 1, idx + 2)
    isSorted [] = True
    isSorted [x] = True
    isSorted (x : y : rest) = x <= y && isSorted (y : rest)
