module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Visualization (drawAppState)
import Controller (initialState, handleEvent, update, AlgorithmChoice(..))

main :: IO ()
main = do
  -- Prompt the user for an array input
  putStrLn "Enter a list of integers separated by spaces (e.g., 5 3 8 1 2):"
  input <- getLine
  let userArray = map read (words input) :: [Int]
  
  -- Initialize the state with the user-defined array and default algorithm
  let initialAlg = Bubble
      state = initialState userArray initialAlg

  -- Start the Gloss window
  let window = InWindow "Sorting Algorithm Visualization" (1000, 700) (100, 100)
      background = white
      fps = 1

  play window background fps state drawAppState handleEvent update