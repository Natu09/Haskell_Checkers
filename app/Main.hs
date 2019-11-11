module Main where

--import Tui
import Checkers

import GameLogic
import Moves
-- import CheckersNathanielHabtegergesa

 
main :: IO ()
main = redAi red_ai applyMove' initialGameState
