module Algorithms.QuickSort (quickSortSteps) where

import Common (SortStep(..))

quickSortSteps :: (Ord a) => [a] -> [SortStep a]
quickSortSteps xs =
    let n = length xs
    in if n <= 1
       then [SortStep xs [] (Nothing, Nothing)]
       else
         let (finalArray, steps) = qs xs 0 (n - 1)
         in steps ++ [SortStep finalArray [] (Nothing, Nothing)]

-- qs sorts the subarray xs[l..r] and returns the sorted array and the steps
qs :: (Ord a) => [a] -> Int -> Int -> ([a], [SortStep a])
qs xs l r
    | l >= r = (xs, [SortStep xs [] (Nothing, Nothing)])
    | otherwise =
        let (xsPart, pIdx, partitionSteps) = partitionLomuto xs l r
            (leftSorted, leftSteps) = qs xsPart l (pIdx - 1)
            (rightSorted, rightSteps) = qs leftSorted (pIdx + 1) r
        in (rightSorted, partitionSteps ++ leftSteps ++ rightSteps)

-- partitionLomuto uses the Lomuto partition scheme and returns the partitioned list,
-- the final pivot index, and the steps taken during partitioning.
partitionLomuto :: (Ord a) => [a] -> Int -> Int -> ([a], Int, [SortStep a])
partitionLomuto xs l r =
    let pivot = xs !! r
        (finalList, iFinal, steps) = go xs l (l - 1) l r pivot []
    in (finalList, iFinal, steps)

-- go performs the partitioning steps. It returns the fully partitioned list segment,
-- the final pivot index, and the steps taken.
go :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> a -> [SortStep a] -> ([a], Int, [SortStep a])
go xs l i j r pivot steps
    | j >= r =
        let i' = i + 1
            xs' = swap xs i' r
            stepSwapPivot = SortStep xs' [r] (Just i', Just r)
        in (xs', i', steps ++ [stepSwapPivot])
    | otherwise =
        let xj = xs !! j
            stepCompare = SortStep xs [r] (Just j, Nothing)
        in if xj <= pivot
           then
               let i' = i + 1
                   xs' = swap xs i' j
                   stepSwap = SortStep xs' [r] (Just i', Just j)
               in go xs' l i' (j + 1) r pivot (steps ++ [stepCompare, stepSwap])
           else
               go xs l i (j + 1) r pivot (steps ++ [stepCompare])

-- Utility to swap two elements in a list
swap :: [a] -> Int -> Int -> [a]
swap xs i j
    | i == j    = xs
    | otherwise =
        let xi = xs !! i
            xj = xs !! j
        in replace j xi (replace i xj xs)

-- replace an element at index idx with value val
replace :: Int -> a -> [a] -> [a]
replace idx val xs =
    let (before, _:after) = splitAt idx xs
    in before ++ (val : after)