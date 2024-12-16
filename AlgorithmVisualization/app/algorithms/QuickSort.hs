module Algorithms.QuickSort (quickSortSteps) where

import Common (SortStep(..))

quickSortSteps :: (Ord a) => [a] -> [SortStep a]
quickSortSteps xs =
    let n = length xs
    in if n <= 1
       then [SortStep {listState = xs, activeIndices = [], currentIndices = (Nothing, Nothing), sortedIndices = [0..n-1]}]
       else
         let (finalArray, steps) = qs xs 0 (n - 1)
         in steps ++ [SortStep {listState = finalArray, activeIndices = [], currentIndices = (Nothing, Nothing), sortedIndices = [0..n-1]}]

qs :: (Ord a) => [a] -> Int -> Int -> ([a], [SortStep a])
qs xs l r
    | l >= r = (xs, [SortStep {listState = xs, activeIndices = [], currentIndices = (Nothing, Nothing), sortedIndices = if l <= r then [l..r] else []}])
    | otherwise =
        let (xsPart, pIdx, partitionSteps) = partitionLomuto xs l r
            (leftSorted, leftSteps) = qs xsPart l (pIdx - 1)
            (rightSorted, rightSteps) = qs leftSorted (pIdx + 1) r
            -- After completely sorting left and right, [l..r] is sorted.
            -- However, to keep changes minimal, we won't remap steps here.
        in (rightSorted, partitionSteps ++ leftSteps ++ rightSteps)

partitionLomuto :: (Ord a) => [a] -> Int -> Int -> ([a], Int, [SortStep a])
partitionLomuto xs l r =
    let pivot = xs !! r
        (finalList, iFinal, steps) = go xs l (l - 1) l r pivot []
    in (finalList, iFinal, steps)

go :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> a -> [SortStep a] -> ([a], Int, [SortStep a])
go xs l i j r pivot steps
    | j >= r =
        let i' = i + 1
            xs' = swap xs i' r
            stepSwapPivot = SortStep { listState = xs'
                                     , activeIndices = [r]
                                     , currentIndices = (Just i', Just r)
                                     , sortedIndices = [i']} -- pivot in correct position
        in (xs', i', steps ++ [stepSwapPivot])
    | otherwise =
        let xj = xs !! j
            stepCompare = SortStep { listState = xs
                                   , activeIndices = [r]
                                   , currentIndices = (Just j, Nothing)
                                   , sortedIndices = [] }
        in if xj <= pivot
           then let i' = i + 1
                    xs' = swap xs i' j
                    stepSwap = SortStep { listState = xs'
                                        , activeIndices = [r]
                                        , currentIndices = (Just i', Just j)
                                        , sortedIndices = [] }
                in go xs' l i' (j+1) r pivot (steps ++ [stepCompare, stepSwap])
           else go xs l i (j+1) r pivot (steps ++ [stepCompare])

swap :: [a] -> Int -> Int -> [a]
swap xs i j
    | i == j    = xs
    | otherwise =
        let xi = xs !! i
            xj = xs !! j
        in replace j xi (replace i xj xs)

replace :: Int -> a -> [a] -> [a]
replace idx val xs =
    let (before, _:after) = splitAt idx xs
    in before ++ (val : after)
