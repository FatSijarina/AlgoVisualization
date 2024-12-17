module Algorithms.MergeSort (mergeSortSteps) where

import Common (SortStep(..))

mergeSortSteps :: (Ord a) => [a] -> [SortStep a]
mergeSortSteps xs = mergeSort xs 0 (length xs - 1)
  where
    mergeSort lst left right
      | left >= right =
          [SortStep { listState = lst
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
      let leftPart  = take (mid - left + 1) (drop left lst)
          rightPart = take (right - mid) (drop (mid+1) lst)
          (steps, finalList) = mergeParts lst left mid right leftPart rightPart
          finalStep = SortStep { listState = finalList
                               , activeIndices = []
                               , currentIndices = (Nothing, Nothing)
                               , sortedIndices = [left..right] }
      in steps ++ [finalStep]

    mergeParts :: (Ord a) => [a] -> Int -> Int -> Int -> [a] -> [a] -> ([SortStep a], [a])
    mergeParts original left mid right leftPart rightPart =
      go original left leftPart rightPart []
      where
        go currentList currentIndex xs ys steps
          | null xs && null ys =
              -- Both sides empty, merging finished
              (steps, currentList)

          | null xs =
              -- Left empty, place remaining right side elements
              let (newList, newSteps) = placeRemaining currentList currentIndex ys steps
              in (newSteps, newList)

          | null ys =
              -- Right empty, place remaining left side elements
              let (newList, newSteps) = placeRemaining currentList currentIndex xs steps
              in (newSteps, newList)

          -- Both xs and ys are non-empty here, so we can pattern match safely
          | (x:xs') <- xs, (y:ys') <- ys =
              let currentLeftIndex = left + (length leftPart - length (x:xs'))
                  currentRightIndex = (mid + 1) + (length rightPart - length (y:ys'))
                  stepCompare = SortStep { listState = currentList
                                         , activeIndices = [currentLeftIndex, currentRightIndex]
                                         , currentIndices = (Just currentLeftIndex, Just currentRightIndex)
                                         , sortedIndices = [] }
              in if x <= y
                 then
                   -- Choose x
                   let updatedList = replaceElement currentList currentIndex x
                       stepPlace = SortStep { listState = updatedList
                                            , activeIndices = [currentIndex]
                                            , currentIndices = (Just currentIndex, Nothing)
                                            , sortedIndices = [] }
                   in go updatedList (currentIndex + 1) xs' ys (steps ++ [stepCompare, stepPlace])
                 else
                   -- Choose y
                   let updatedList = replaceElement currentList currentIndex y
                       stepPlace = SortStep { listState = updatedList
                                            , activeIndices = [currentIndex]
                                            , currentIndices = (Just currentIndex, Nothing)
                                            , sortedIndices = [] }
                   in go updatedList (currentIndex + 1) xs ys' (steps ++ [stepCompare, stepPlace])

    -- placeRemaining places leftover elements (from one half) without comparisons
    placeRemaining :: [a] -> Int -> [a] -> [SortStep a] -> ([a], [SortStep a])
    placeRemaining currentList currentIndex elems steps =
      let (finalList, finalSteps, _) =
            foldl (\(cl, st, i) val ->
                     let updated = replaceElement cl i val
                         stepPlace = SortStep { listState = updated
                                              , activeIndices = [i]
                                              , currentIndices = (Just i, Nothing)
                                              , sortedIndices = [] }
                     in (updated, st ++ [stepPlace], i+1))
                  (currentList, steps, currentIndex)
                  elems
      in (finalList, finalSteps)

    replaceElement :: [a] -> Int -> a -> [a]
    replaceElement lst idx val =
      take idx lst ++ [val] ++ drop (idx + 1) lst
