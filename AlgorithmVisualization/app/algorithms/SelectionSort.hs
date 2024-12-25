module Algorithms.SelectionSort (selectionSortSteps) where

import Common (SortStep(..))

selectionSortSteps :: (Ord a) => [a] -> [SortStep a]
selectionSortSteps xs = go xs 0 [] []
  where
    go lst i acc sorted
      | i >= length lst - 1 = reverse (SortStep lst [] (Nothing, Nothing) (sorted ++ [i]) : acc)
      | otherwise =
          let minIndex = findMinIndex i lst
              stepFindingMin = SortStep lst [i, minIndex] (Just i, Just minIndex) sorted
              newLst = swap i minIndex lst
              newSorted = sorted ++ [i] 
              stepSwapping = SortStep newLst [i, minIndex] (Just i, Just minIndex) newSorted
          in go newLst (i + 1) (stepSwapping : stepFindingMin : acc) newSorted

    findMinIndex i lst = snd $ minimum $ zip (drop i lst) [i..]

    swap i j lst =
      let elemI = lst !! i
          elemJ = lst !! j
      in replace i elemJ (replace j elemI lst)

    replace i x lst = take i lst ++ [x] ++ drop (i + 1) lst