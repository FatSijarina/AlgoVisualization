module Algorithms.MergeSort (mergeSortSteps) where

import Common (SortStep(..))

-- Merge sort implementation with steps
mergeSortSteps :: (Ord a) => [a] -> [SortStep a]
mergeSortSteps xs = mergeSort xs 0 (length xs - 1)
  where
    mergeSort :: (Ord a) => [a] -> Int -> Int -> [SortStep a]
    mergeSort lst left right
      | left >= right = [SortStep lst []] -- Single-element or empty range
      | otherwise =
          let mid = (left + right) `div` 2
              leftSteps = mergeSort lst left mid
              rightSteps = mergeSort (listState (last leftSteps)) (mid + 1) right
              mergedSteps = merge (listState (last rightSteps)) left mid right
          in leftSteps ++ rightSteps ++ mergedSteps

    merge :: (Ord a) => [a] -> Int -> Int -> Int -> [SortStep a]
    merge lst left mid right =
      let leftPart = take (mid - left + 1) (drop left lst)
          rightPart = take (right - mid) (drop (mid + 1) lst)
          merged = mergeParts leftPart rightPart left lst
      in merged

    mergeParts :: (Ord a) => [a] -> [a] -> Int -> [a] -> [SortStep a]
    mergeParts [] [] _ lst = [SortStep lst []]
    mergeParts [] ys idx lst = go ys idx lst []
    mergeParts xs [] idx lst = go xs idx lst []
    mergeParts (x:xs) (y:ys) idx lst
      | x <= y =
          let updatedList = replaceElement lst idx x
              step = SortStep updatedList [idx]
          in step : mergeParts xs (y:ys) (idx + 1) updatedList
      | otherwise =
          let updatedList = replaceElement lst idx y
              step = SortStep updatedList [idx]
          in step : mergeParts (x:xs) ys (idx + 1) updatedList

    go :: (Ord a) => [a] -> Int -> [a] -> [SortStep a] -> [SortStep a]
    go [] _ lst steps = steps
    go (z:zs) idx lst steps =
      let updatedList = replaceElement lst idx z
          step = SortStep updatedList [idx]
      in go zs (idx + 1) updatedList (steps ++ [step])

    replaceElement :: [a] -> Int -> a -> [a]
    replaceElement lst idx val =
      take idx lst ++ [val] ++ drop (idx + 1) lst