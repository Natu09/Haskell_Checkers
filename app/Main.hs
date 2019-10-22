module Main where

--import Tui
import Checkers

import GameLogic
 
main :: IO ()
main = tui

-- simple_moves :: GameState -> [Move]
-- simple_moves s 
--     | s^.status == Red = red_move s
--     | s^.status == Black = black_move s
--     | otherwise = s

-- red_move :: GameState -> [Move]
-- red_move s = [[x,y] | x <- _redPieces s,
--                       y <- map(\(x,y) -> (x-1,y-1)) _redPieces]

-- -- (x-1,y+1), (x+1,y+1)

-- black_move :: GameState -> [Move]
-- black_move s = [[x,y] | x <- _blackPieces s, 
--                         y <- map(\(x,y) -> (x-1,y+1)) _blackPieces]
-- --  
-- --m s = case s^.status of
--   --Red -> setMessage $ set status Red (newGameState) -- set redPieces ((head m):(s^.redPieces))
--   --Black -> 



-- apply_move :: GameState -> Move -> GameState
-- apply_move s m = case s^.status of
--   Red -> if m `elem` (simple_moves s) then set redPieces ((last m):(s^.redPieces))
--   Black -> if m `elem` (simple_moves s) then set blackPieces ((last m):(s^.blackPieces))
--   _ -> initialGameState  

-- -- if m is valid then set message and status to black/red then apply_moves m s

-- jump_moves :: GameState -> [Move]
-- jump_moves = undefined 

-- pawnmove :: 

-- human myApplyMove myGameState

-- over redPieces (add (1,2)) (over redPieces (delete (1,2)) g)
-- over - hand in a function
-- over parameter f g = set (f (view parameter g)) g
-- dont need too do this tho, record notation works fine


-- HINTS (just one way you could do it)

-- pawnMove :: (Coord, GameState) -> [(Coord, GameState)]


-- data RoseTree a = Node a [RoseTree a]
-- what you might want to think about: 
        -- how would you unfold + fold work for this data type?
    -- [(Coord, GameState)]

