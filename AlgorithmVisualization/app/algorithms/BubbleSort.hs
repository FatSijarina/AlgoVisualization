module Algorithms.BubbleSort (bubbleSortSteps) where

import Common (SortStep(..))

bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = go xs 0 []
  where
    go lst pass acc
      | isSorted lst =
          reverse (SortStep { listState = lst
                            , activeIndices = []
                            , currentIndices = (Nothing, Nothing)
                            , sortedIndices = [0..length lst - 1]} : acc)
      | otherwise =
          let (newList, newSteps) = bubblePass lst 0 (length lst - pass - 1) []
              n = length newList
              sortedSoFar = [n - (pass + 1) .. n - 1]
              endStep = SortStep { listState = newList
                                 , activeIndices = []
                                 , currentIndices = (Nothing, Nothing)
                                 , sortedIndices = sortedSoFar }
          in go newList (pass + 1) (endStep : newSteps ++ acc)

    bubblePass :: (Ord a) => [a] -> Int -> Int -> [SortStep a] -> ([a], [SortStep a])
    bubblePass lst start end steps
      | start >= end = (lst, steps)
      | otherwise =
          let i = start
              j = start + 1
              x = lst !! i
              y = lst !! j
              stepCompare = SortStep { listState = lst
                                     , activeIndices = [i, j]
                                     , currentIndices = (Just i, Just j)
                                     , sortedIndices = [] }
          in if x > y
             then
               let swapped = swap lst i j
                   stepSwap = SortStep { listState = swapped
                                       , activeIndices = [i, j]
                                       , currentIndices = (Just i, Just j)
                                       , sortedIndices = [] }
               in bubblePass swapped (start+1) end (stepSwap : stepCompare : steps)
             else
               bubblePass lst (start+1) end (stepCompare : steps)

    swap :: [a] -> Int -> Int -> [a]
    swap arr i j
      | i == j = arr
      | otherwise =
          let xi = arr !! i
              xj = arr !! j
          in replace j xi (replace i xj arr)

    replace :: Int -> a -> [a] -> [a]
    replace idx val xs =
      let (before, _:after) = splitAt idx xs
      in before ++ (val : after)

    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)