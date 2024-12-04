module Algorithms (bubbleSortSteps) where

bubbleSortSteps :: (Ord a) => [a] -> [[a]]
bubbleSortSteps xs = go xs []
  where
    go lst acc
      | isSorted lst = reverse (lst : acc)
      | otherwise    = go (bubble lst) (lst : acc)

    bubble (x:y:rest)
      | x > y     = y : bubble (x:rest)
      | otherwise = x : bubble (y:rest)
    bubble lst = lst

    isSorted [] = True

    isSorted [x] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)