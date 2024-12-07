module Common (SortStep(..)) where

data SortStep a = SortStep
  { listState     :: [a]
  , activeIndices :: [Int]
  } deriving (Show)