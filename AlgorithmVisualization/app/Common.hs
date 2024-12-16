module Common (SortStep(..)) where

data SortStep a = SortStep
  { listState      :: [a]
  , activeIndices  :: [Int]
  , currentIndices :: (Maybe Int, Maybe Int)  -- Indices of currently active elements
  , sortedIndices  :: [Int]                   -- Indices of definitively sorted elements
  } deriving (Show)
