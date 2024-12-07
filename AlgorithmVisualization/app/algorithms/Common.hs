module Algorithms.Common (SortStep(..)) where

-- Common data type for sorting steps
data SortStep a = SortStep
  { listState     :: [a]
  , activeIndices :: [Int]
  } deriving (Show)