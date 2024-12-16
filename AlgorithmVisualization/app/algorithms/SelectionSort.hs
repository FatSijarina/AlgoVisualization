module Algorithms.SelectionSort (selectionSortSteps) where

import Common (SortStep(..))

selectionSortSteps :: (Ord a) => [a] -> [SortStep a]
selectionSortSteps xs = go xs 0 [] []  -- Include an accumulator for sorted elements
  where
    -- Recursive function to generate the steps
    go lst i acc sorted
      | i >= length lst - 1 = reverse (SortStep lst [] (Nothing, Nothing) (sorted ++ [i]) : acc)  -- Final step includes the last sorted index
      | otherwise =
          let minIndex = findMinIndex i lst
              -- Record step when finding the minimum element
              stepFindingMin = SortStep lst [i, minIndex] (Just i, Just minIndex) sorted
              -- Perform the swap and track the updated sorted list
              newLst = swap i minIndex lst
              newSorted = sorted ++ [i]  -- Mark current index as sorted
              stepSwapping = SortStep newLst [i, minIndex] (Just i, Just minIndex) newSorted
          in go newLst (i + 1) (stepSwapping : stepFindingMin : acc) newSorted

    -- Find the index of the minimum element in the unsorted portion of the list
    findMinIndex i lst = snd $ minimum $ zip (drop i lst) [i..]

    -- Swap two elements in the list
    swap i j lst =
      let elemI = lst !! i
          elemJ = lst !! j
      in replace i elemJ (replace j elemI lst)

    -- Replace an element in the list at the given index
    replace i x lst = take i lst ++ [x] ++ drop (i + 1) lst