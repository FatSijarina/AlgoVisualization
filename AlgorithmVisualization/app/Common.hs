module Common (SortStep(..)) where

data SortStep a = SortStep
  { listState      :: [a]
  , activeIndices  :: [Int]
  , currentIndices :: (Maybe Int, Maybe Int) 
  , sortedIndices  :: [Int]
  } deriving (Show)
