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
         let (finalArray, steps) = qs xs 0 (n - 1) []
         in steps ++ [SortStep { listState = finalArray
                               , activeIndices = []
                               , currentIndices = (Nothing, Nothing)
                               , sortedIndices = [0..n-1]}]

qs :: (Ord a) => [a] -> Int -> Int -> [Int] -> ([a], [SortStep a])
qs xs l r globalSorted
    | l >= r =
        let newSorted = if l <= r then globalSorted ++ [l..r] else globalSorted
        in (xs, [SortStep {listState = xs
                          , activeIndices = []
                          , currentIndices = (Nothing, Nothing)
                          , sortedIndices = newSorted}])
    | otherwise =
        let (partList, pivotIndex, partSteps) = partitionLomuto xs l r globalSorted
            updatedSorted = globalSorted ++ [pivotIndex]
            (leftSorted, leftSteps) = qs partList l (pivotIndex - 1) updatedSorted
            (rightSorted, rightSteps) = qs leftSorted (pivotIndex + 1) r updatedSorted
            finalSorted = updatedSorted ++ [l..r] 
            finalState = SortStep { listState = rightSorted
                                  , activeIndices = []
                                  , currentIndices = (Nothing, Nothing)
                                  , sortedIndices = finalSorted }
        in (rightSorted, partSteps ++ leftSteps ++ rightSteps ++ [finalState])

partitionLomuto :: (Ord a) => [a] -> Int -> Int -> [Int] -> ([a], Int, [SortStep a])
partitionLomuto xs l r globalSorted =
    let pivot = xs !! r
        (finalList, iFinal, steps) = partitionGo xs l (l-1) l r pivot [] globalSorted
    in (finalList, iFinal, steps)

partitionGo :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> a -> [SortStep a] -> [Int] -> ([a], Int, [SortStep a])
partitionGo xs l i j r pivot steps globalSorted
    | j >= r =
        let i' = i + 1
            xs' = swap xs i' r
            updatedSorted = globalSorted ++ [i']
            stepPivot = SortStep { listState = xs'
                                 , activeIndices = []
                                 , currentIndices = (Just i', Just r)
                                 , sortedIndices = updatedSorted }
        in (xs', i', steps ++ [stepPivot])
    | otherwise =
        let xj = xs !! j
            stepCompare = SortStep { listState = xs
                                   , activeIndices = [j, r]
                                   , currentIndices = (Just j, Just r)
                                   , sortedIndices = globalSorted }
        in if xj <= pivot
           then 
              let i' = i + 1
                  xs' = swap xs i' j
                  stepSwap = SortStep { listState = xs'
                                      , activeIndices = [i', j]
                                      , currentIndices = (Just i', Just j)
                                      , sortedIndices = globalSorted }
              in partitionGo xs' l i' (j+1) r pivot (steps ++ [stepCompare, stepSwap]) globalSorted
           else
              partitionGo xs l i (j+1) r pivot (steps ++ [stepCompare]) globalSorted

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
         [] -> before
         (_:after) -> before ++ (val : after)