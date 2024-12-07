module Algorithms.BubbleSort (bubbleSortSteps, SortStep(..)) where

-- Data type for sorting steps
data SortStep a = SortStep
  { listState     :: [a]
  , activeIndices :: [Int]
  } deriving (Show)

-- Bubble sort implementation returning sorting steps
bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = map (\lst -> SortStep lst []) (go xs [])
  where
    go lst acc
      | isSorted lst = reverse (lst : acc)
      | otherwise    = go (bubble lst) (lst : acc)
    bubble [x] = [x]
    bubble (x:y:rest)
      | x > y     = y : bubble (x:rest)
      | otherwise = x : bubble (y:rest)
    isSorted [] = True
    isSorted [x] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)
