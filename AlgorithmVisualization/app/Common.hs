module Common (SortStep(..)) where

data SortStep a = SortStep
  { listState     :: [a]
  , activeIndices :: [Int]
  , currentIndices :: (Maybe Int, Maybe Int)  -- Shton indeksin aktual të i dhe j
  } deriving (Show)