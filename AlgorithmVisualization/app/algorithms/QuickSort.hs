module Algorithms.QuickSort (quickSortSteps) where

import Common (SortStep(..))

quickSortSteps :: (Ord a) => [a] -> [SortStep a]
quickSortSteps xs =
    let n = length xs
    in if n <= 1
       then [SortStep { listState = xs
                      , activeIndices = []
                      , currentIndices = (Nothing, Nothing)
                      , sortedIndices = [0..n-1]}]
       else
         let (finalArray, steps) = qs xs 0 (n - 1)
         in steps ++ [SortStep { listState = finalArray
                               , activeIndices = []
                               , currentIndices = (Nothing, Nothing)
                               , sortedIndices = [0..n-1]}]

qs :: (Ord a) => [a] -> Int -> Int -> ([a], [SortStep a])
qs xs l r
    | l >= r = (xs, [SortStep {listState = xs
                              , activeIndices = []
                              , currentIndices = (Nothing, Nothing)
                              , sortedIndices = if l <= r then [l..r] else []}])
    | otherwise =
        let (partList, pivotIndex, partSteps) = partitionLomuto xs l r
            (leftSorted, leftSteps) = qs partList l (pivotIndex - 1)
            (rightSorted, rightSteps) = qs leftSorted (pivotIndex + 1) r
            -- After sorting the sub-parts, [l..r] is fully sorted
            finalState = SortStep { listState = rightSorted
                                  , activeIndices = []
                                  , currentIndices = (Nothing, Nothing)
                                  , sortedIndices = [l..r] }
        in (rightSorted, partSteps ++ leftSteps ++ rightSteps ++ [finalState])

partitionLomuto :: (Ord a) => [a] -> Int -> Int -> ([a], Int, [SortStep a])
partitionLomuto xs l r =
    let pivot = xs !! r
        (finalList, iFinal, steps) = partitionGo xs l (l-1) l r pivot []
    in (finalList, iFinal, steps)
-- partitionGo performs the Lomuto partition, but produces comparison and placement steps incrementally.
partitionGo :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> a -> [SortStep a] -> ([a], Int, [SortStep a])
partitionGo xs l i j r pivot steps
    | j >= r =
        -- Place pivot in correct position
        let i' = i + 1
            xs' = swap xs i' r
            -- Final pivot placement step: pivot is now at i', which is sorted
            stepPivot = SortStep { listState = xs'
                                 , activeIndices = [i']
                                 , currentIndices = (Just i', Just r)
                                 , sortedIndices = [i'] }
        in (xs', i', steps ++ [stepPivot])
    | otherwise =
        let xj = xs !! j
            -- Comparison step: highlight comparing element at j with pivot at r
            stepCompare = SortStep { listState = xs
                                   , activeIndices = [j, r]
                                   , currentIndices = (Just j, Just r)
                                   , sortedIndices = [] }
        in if xj <= pivot
           then 
              let i' = i + 1
                  xs' = swap xs i' j
                  -- Placement step: place element xj in correct position
                  stepSwap = if i' /= j
                             then SortStep { listState = xs'
                                           , activeIndices = [i', j]
                                           , currentIndices = (Just i', Just j)
                                           , sortedIndices = [] }
                             else -- If i' == j, no change in array, but still show a step for clarity
                                  SortStep { listState = xs'
                                           , activeIndices = [i']
                                           , currentIndices = (Just i', Just j)
                                           , sortedIndices = [] }
              in partitionGo xs' l i' (j+1) r pivot (steps ++ [stepCompare, stepSwap])
           else
              -- xj > pivot, no swap needed, just move on
              partitionGo xs l i (j+1) r pivot (steps ++ [stepCompare])

swap :: [a] -> Int -> Int -> [a]
swap xs i j
    | i == j    = xs
    | otherwise =
        let xi = xs !! i
            xj = xs !! j
        in replace j xi (replace i xj xs)

replace :: Int -> a -> [a] -> [a]
replace idx val xs =
    let (before, after') = splitAt idx xs
    in case after' of
         [] -> before -- idx out of range would be a bug, but let's assume correct usage
         (_:after) -> before ++ (val : after)