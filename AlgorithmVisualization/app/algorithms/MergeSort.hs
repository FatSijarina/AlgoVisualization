module Algorithms.MergeSort (mergeSortSteps) where

import Common (SortStep(..))

mergeSortSteps :: (Ord a) => [a] -> [SortStep a]
mergeSortSteps xs = mergeSort xs 0 (length xs - 1)
  where
    mergeSort lst left right
      | left >= right = [SortStep { listState = lst
                                  , activeIndices = []
                                  , currentIndices = (Nothing, Nothing)
                                  , sortedIndices = if left <= right then [left..right] else []}]
      | otherwise =
          let mid = (left + right) `div` 2
              leftSteps = mergeSort lst left mid
              rightSteps = mergeSort (listState (last leftSteps)) (mid+1) right
              mergedSteps = merge (listState (last rightSteps)) left mid right
          in leftSteps ++ rightSteps ++ mergedSteps

    merge lst left mid right =
      let leftPart = take (mid - left + 1) (drop left lst)
          rightPart = take (right - mid) (drop (mid+1) lst)
          mergedSteps = mergeParts leftPart rightPart left lst
          -- After merging, [left..right] is sorted
      in map (\s -> s { sortedIndices = [left..right] }) mergedSteps

    mergeParts [] [] _ lst = [SortStep { listState = lst
                                       , activeIndices = []
                                       , currentIndices = (Nothing, Nothing)
                                       , sortedIndices = []}] 
    mergeParts [] ys idx lst = go ys idx lst []
    mergeParts xs [] idx lst = go xs idx lst []
    mergeParts (x:xs) (y:ys) idx lst
      | x <= y =
          let updatedList = replaceElement lst idx x
              step = SortStep { listState = updatedList
                              , activeIndices = [idx]
                              , currentIndices = (Just idx, Nothing)
                              , sortedIndices = []}
          in step : mergeParts xs (y:ys) (idx+1) updatedList
      | otherwise =
          let updatedList = replaceElement lst idx y
              step = SortStep { listState = updatedList
                              , activeIndices = [idx]
                              , currentIndices = (Just idx, Nothing)
                              , sortedIndices = []}
          in step : mergeParts (x:xs) ys (idx+1) updatedList

    go [] _ lst steps = steps
    go (z:zs) idx lst steps =
      let updatedList = replaceElement lst idx z
          step = SortStep { listState = updatedList
                          , activeIndices = [idx]
                          , currentIndices = (Just idx, Nothing)
                          , sortedIndices = []}
      in go zs (idx+1) updatedList (steps ++ [step])

    replaceElement lst idx val = take idx lst ++ [val] ++ drop (idx+1) lst
