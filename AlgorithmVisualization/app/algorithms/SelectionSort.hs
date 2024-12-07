module Algorithms.SelectionSort (selectionSortSteps) where

import Common (SortStep(..))

selectionSortSteps :: (Ord a) => [a] -> [SortStep a]
selectionSortSteps xs = go xs 0 []
  where
    go lst i acc
      | i >= length lst - 1 = reverse (SortStep lst [] (Nothing, Nothing) : acc)
      | otherwise =
          let (minIndex, newLst) = selectAndSwap i lst
              step = SortStep newLst [i, minIndex] (Just i, Just minIndex)
          in go newLst (i + 1) (step : acc)

    selectAndSwap i lst =
      let minIndex = findMinIndex i lst
          newLst = swap i minIndex lst
      in (minIndex, newLst)

    findMinIndex i lst = snd $ minimum $ zip (drop i lst) [i..]

    swap i j lst =
      let elemI = lst !! i
          elemJ = lst !! j
      in replace i elemJ (replace j elemI lst)

    replace i x lst = take i lst ++ [x] ++ drop (i + 1) lst
