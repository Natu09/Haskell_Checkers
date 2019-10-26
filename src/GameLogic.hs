{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module GameLogic where

import Lens.Micro.Platform


type ApplyMove = Move -> GameState -> GameState

type AiMove = GameState -> Move

data MoveType = Human | AI AiMove

type Coord = (Int, Int)
type Move = [Coord]

data Status = Red | Black | GameOver 
  deriving (Show, Eq)


data GameState =
  GameState { _blackPieces :: [Coord]
            , _redPieces :: [Coord]
            , _blackKings :: [Coord]
            , _redKings :: [Coord]
            , _status :: Status
            , _message :: String}
              deriving (Show, Eq)

makeLenses ''GameState


initialGameState :: GameState
initialGameState =
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = ""}

blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]

setMessage :: GameState -> GameState
setMessage s = case (s^.status) of
  Red -> set message
    "Red Turn." s
  Black -> set message
    "Black Turn." s
  _ -> s
  
-- applyMove :: Move -> GameState -> GameState
-- applyMove _  s = case s^.status of
--   Red -> setMessage $ set status Black s
--   Black -> setMessage $ set status Red s
--   _ -> initialGameState



-------------MY CODE ---------------------------------
   














applyMove :: Move -> GameState -> GameState
applyMove m s = case s^.status of
    Red -> setMessage $ set status Black (apply_move s m) 
    Black -> setMessage $ set status Red (apply_move s m) 
    _ -> initialGameState

    
-- A function to collect a list of all posible moves for a red/black piece
simple_moves :: GameState -> [Move]
simple_moves s =
    if s^.status == Red then red_move s
    else if  s^.status == Black then black_move s
    else [[]]

red_move :: GameState -> [Move]
red_move s = [[(x,y),(a,b)] | (x,y) <- _redPieces s,
                              (a,b) <- [(x-1,y-1),(x+1,y-1)]]

black_move :: GameState -> [Move]
black_move s = [[(x,y),(a,b)]  |  (x,y) <- _blackPieces s, 
                                  (a, b) <- [(x+1,y+1), (x-1,y+1)]]
                                
apply_move :: GameState -> Move -> GameState
apply_move s m = case s^.status of
  Red -> if m `elem` (simple_moves s) then s{_redPieces = (last m):[x | x <- _redPieces s, not (head m == x)]} else s
  Black -> if m `elem` (simple_moves s) then s{_blackPieces = (last m):[x | x <- _blackPieces s, not (head m == x)]} else s
  _ -> initialGameState  
  

   