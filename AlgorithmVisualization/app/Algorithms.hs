module Algorithms (bubbleSortSteps, SortStep(..)) where

-- Define a new data type to hold sorting steps
data SortStep a = SortStep
  { listState     :: [a]
  , activeIndices :: [Int]
  } deriving (Show)

bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = go xs []
  where
    go lst acc
      | isSorted lst = reverse (SortStep lst [] : acc)
      | otherwise    = go (bubble lst) (SortStep lst active : acc)
      where
        active = findActiveIndices lst
        bubble (x:y:rest)
          | x > y     = y : bubble (x:rest)
          | otherwise = x : bubble (y:rest)
        bubble lst = lst

    findActiveIndices lst = [0, 1] -- Simplify: always highlight first two elements
    isSorted [] = True
    isSorted [x] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)