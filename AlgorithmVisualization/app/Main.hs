module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Visualization (drawAppState)
import Controller (initialState, handleEvent, update, AlgorithmChoice(..))

main :: IO ()
main = do
  let initialAlg = Bubble
      state = initialState [] initialAlg

  let window = InWindow "Sorting Algorithm Visualization" (1000, 700) (100, 100)
      background = white
      fps = 1

  play window background fps state drawAppState handleEvent update