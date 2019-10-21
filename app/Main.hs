module Main where

--import Tui
import Checkers
 
main :: IO ()
main = tui

{-
simple_moves :: Move -> GameState -> GameState
simple_moves m s 
    | s^.status == Red = red_piece m 
    | s^.status == Black = black_piece m
    | otherwise = s

red_piece :: 

--  
--m s = case s^.status of
  --Red -> setMessage $ set status Red (newGameState) -- set redPieces ((head m):(s^.redPieces))
  --Black -> 
-}



-- applyMove :: Move -> GameState -> GameState
-- applyMove m s = case s^.status of
--   Red -> setMessage $ set status Red (newGameState) -- set redPieces ((head m):(s^.redPieces))
--   Black -> setMessage $ set status Black (newGameState) -- set blackPieces ((head m):(s^.blackPieces))
--   _ -> initialGameState

-- -- if m is valid then set message and status to black/red then apply_moves m s

-- newGameState :: GameState
-- newGameState =
--   GameState { _blackPieces = blackInit
--             , _redPieces = (2,3):redInit -- set redPieces ((head m):(s^.redPieces))
--             , _blackKings = []
--             , _redKings = []
--             , _status = Red
--             , _message = ""}

-- simple_moves :: GameState -> [Move] 
-- simple_moves = undefined


-- jump_moves :: GameState -> [Move]
-- jump_moves = undefined

-- apply_move :: GameState -> Move -> GameState
-- apply_move = undefined 
 
-- pawnmove :: 

-- human myApplyMove myGameState


-- s c c m

-- left_move :: Status -> [Coord] -> [Coord] -> [Move]
-- left_move w x y = undefined  

-- right_move :: Move -> GameState -> GameState
-- right_move _ s


