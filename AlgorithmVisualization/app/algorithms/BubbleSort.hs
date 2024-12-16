module Algorithms.BubbleSort (bubbleSortSteps) where

import Common (SortStep(..))

bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = go xs 0 []
  where
    go lst pass acc
      | isSorted lst =
          reverse (SortStep { listState = lst
                            , activeIndices = []
                            , currentIndices = (Nothing, Nothing)
                            , sortedIndices = [0..length lst - 1]} : acc)
      | otherwise =
          let (newList, active, i, j) = bubble lst 0
              n = length newList
              -- After pass-th iteration, last (pass+1) elements are sorted
              sortedSoFar = [n - (pass + 1) .. n - 1]
          in go newList (pass + 1) (SortStep { listState = newList
                                             , activeIndices = active
                                             , currentIndices = (Just i, Just j)
                                             , sortedIndices = sortedSoFar} : acc)

    bubble [x] _ = ([x], [], -1, -1)
    bubble (x:y:rest) idx
      | x > y =
          let (bubbled, _, i, j) = bubble (x:rest) (idx+1)
          in (y:bubbled, [idx, idx+1], idx, idx+1)
      | otherwise =
          let (bubbled, _, i, j) = bubble (y:rest) (idx+1)
          in (x:bubbled, [idx+1], idx+1, idx+2)

    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)
