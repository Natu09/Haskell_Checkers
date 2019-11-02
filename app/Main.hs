module Main where

--import Tui
import Checkers

-- import GameLogic

import Moves
 
main :: IO ()
main = human applyMove' initialGameState

