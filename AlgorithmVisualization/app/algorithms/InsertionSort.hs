module Algorithms.InsertionSort (insertionSortSteps) where

import Common (SortStep(..))

insertionSortSteps :: (Ord a) => [a] -> [SortStep a]
insertionSortSteps xs = go xs 1 [] [0]
  where
    go lst i acc sorted
      | i >= length lst =
          -- All elements are inserted and the entire array is sorted
          reverse (SortStep lst [] (Nothing, Nothing) [0..length lst - 1] : acc)
      | otherwise =
          let key        = lst !! i
              j          = i - 1
              (finalList, newAcc, finalSorted) = insertKey lst i j key acc sorted
          in go finalList (i + 1) newAcc finalSorted

    insertKey lst i j key acc sorted
      | j < 0 =
          -- Key should be at the start
          let finalList  = shiftAndInsert lst key 0 i
              finalSorted = [0..i]
              stepInsert  = SortStep finalList [0, i] (Just 0, Just i) finalSorted
          in (finalList, stepInsert : acc, finalSorted)
      | otherwise =
          let currentVal = lst !! j
              stepCompare = SortStep lst [j, i] (Just j, Just i) sorted
          in if currentVal > key
             then
                 -- Keep comparing further left, no changes to the array yet
                 insertKey lst i (j-1) key (stepCompare : acc) sorted
             else
                 -- Found insertion position at j+1
                 let pos        = j + 1
                     finalList   = shiftAndInsert lst key pos i
                     finalSorted = [0..i]
                     stepInsert  = SortStep finalList [pos, i] (Just pos, Just i) finalSorted
                 in (finalList, stepInsert : stepCompare : acc, finalSorted)

    -- shiftAndInsert:
    -- Remove the key from position i and insert it at position pos.
    -- This simulates the "shifting" of elements to the right, but done in one final step.
    shiftAndInsert :: [a] -> a -> Int -> Int -> [a]
    shiftAndInsert lst key pos i =
      let -- Remove the key at index i:
          lstWithoutKey = take i lst ++ drop (i + 1) lst
          -- Insert the key at position pos:
          finalList     = take pos lstWithoutKey ++ [key] ++ drop pos lstWithoutKey
      in finalList
