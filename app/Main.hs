module Main where

--import Tui
import Checkers

-- import GameLogic

import Moves


 
main :: IO ()
main = aiTest black_ai red_ai applyMove' initialGameState
