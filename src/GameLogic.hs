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


------------------------------------------------


initialGameState2 :: GameState
initialGameState2 =
  GameState { _blackPieces = blackInit2
            , _redPieces = redInit2
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = ""}




blackInit2 :: [Coord]
blackInit2 = [ (0,7), (2,7), (4,7), (6,7)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,4), (3,2), (5,2), (7,2)]

redInit2 :: [Coord]
redInit2 = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (1,2), (2,5), (4,5), (6,5)]











-------------MY CODE ---------------------------------
   
















    


-- A function to collect a list of all posible moves for a red/black piece
simple_moves :: GameState -> [Move]
simple_moves s =
      if _status s == Red then (simpleKing (_redKings s)) ++ (simplePiece (_redPieces s)) --red_move s
      else if  _status s == Black then (simpleKing (_blackKings s)) ++ (simplePiece (_blackPieces s))
      else [[]]
    where 
      simplePiece xs = [[(x,y),(x',y')] | (x,y) <- xs,
                                          (x',y') <- [(x+1, y+dir), (x-1, y+dir)],
                                          emptyposition (x', y') s && onboard (x', y')]
                                          
      simpleKing xs = [[(x,y),(x',y')] | (x,y) <- xs,
                                         (x', y') <- [ (x+1,y+1), (x-1,y+1), (x+1,y-1),(x-1,y-1) ],
                                         emptyposition (x', y') s && onboard (x', y')]

      dir = case (_status s) of {Red -> -1; Black -> 1}
      emptyposition pos st = not (member pos (_blackKings st)) && not (member pos (_redKings st)) && not (member pos (_blackPieces st)) && not (member pos (_redPieces st))
      onboard pos = if (fst pos < 8 && fst pos >= 0) && (snd pos < 8 && snd pos >= 0)  then True else False

-- member :: GameState -> [Coord] -> Bool
member x [] = False
member x (a:as)
  | x == a = True
  | otherwise = member x as

remove :: Coord -> [Coord] -> [Coord] 
remove start ls = [x | x <- ls, start /= x]

replace :: Coord -> Coord -> [Coord] -> [Coord]
replace start end [] = []
replace start end (a:as)
  | a == start = end:as
  | otherwise = a:(replace start end as)
  
opponent_occupied :: Coord -> GameState -> Bool 
opponent_occupied pos st 
  | _status st == Red = if (member pos (_blackKings st)) || (member pos (_blackPieces st))  then True else False
  | _status st == Black = if (member pos (_redKings st)) || (member pos (_redPieces st)) then True else False
  | otherwise = False

jump_moves :: GameState -> [Move]
jump_moves st 
      | _status st == Red = (jumpKing (_redKings st)) ++ (jumpPiece (_redPieces st)) --red_move s
      | otherwise = (jumpKing (_blackKings st)) ++ (jumpPiece (_blackPieces st))
    where 
      jumpPiece xs =  [(x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y)]
      jumpPiece' start rem (x,y) =
                    [(x'',y''):ys
                    | ((x',y'),(x'',y'')) <- [((x+1,y+dir),(x+2,y+dir+dir)),((x-1,y+dir),(x-2,y+dir+dir))],
                    (not (member (x',y') rem)) && (opponent_occupied (x',y') st) && (start==(x'',y'') || (notoccupied (x'',y'') st && onboard (x'',y''))),
                    ys <- jump_over (jumpPiece' start ((x',y'):rem) (x'',y'')) ]

      jumpKing xs = [(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y)]
      jumpKing' start rem (x,y) =
                    [ (x'',y''):ys
                    | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                    , (not (member (x',y') rem)) && (opponent_occupied (x',y') st) && (start==(x'',y'') || (notoccupied (x'',y'') st && onboard (x'',y'')))
                    , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'')) ] 
  
      dir = case (_status st) of {Red -> -1; Black -> 1}
      notoccupied pos st = not (member pos (_blackKings st)) && not (member pos (_redKings st)) && not (member pos (_blackPieces st)) && not(member pos (_redPieces st))
      onboard pos = if (fst pos < 8 && fst pos >= 0) && (snd pos < 8 && snd pos >= 0)  then True else False
      jump_over [] = [[]]
      jump_over z = z

      

moves st 
  | jumpmoves /= [] = jumpmoves
  | otherwise = simplemoves
    where
      simplemoves = simple_moves st
      jumpmoves = jump_moves st

changeplayer :: GameState -> GameState
changeplayer st
  | (_status st) == Black = set status Red st
  | otherwise = set status Black st


applyMove :: Move -> GameState -> GameState
applyMove m s
    | m `elem` (moves s) = if m `elem` (jump_moves s) then (apply_jump m s) else (apply_simple m s)
    | otherwise = s {_message = "Illegal Move!"}


apply_simple :: Move -> GameState -> GameState
apply_simple [start, end] st
                    | _status st == Red && member start (_redKings st) = 
                    st {_redKings = replace start end (_redKings st), 
                        _status = change (_status st) } {_message = "Blacks Turn!"}
                    | _status st == Black && member start (_blackKings st) =
                    st {_blackKings = replace start end (_blackKings st),
                        _status = change (_status st) } {_message = "Reds Turn!"}
                    |  _status st == Red && member start (_redPieces st) = 
                    -- if (snd end == 0) then
                    st {_redPieces = replace start end (_redKings st),
                          _status = change (_status st) } {_message = "Blacks Turn!"}
                    -- else 
                    -- st {_redPieces = replace start end (_redPieces st),
                    --       _status = change (_status st) } {_message = "Blacks Turn!"}
                    | _status st == Black && member start (_blackPieces st) =
                   -- if (snd end == 7) then
                    st {_blackPieces  = replace start end (_blackKings st),
                          _status = change (_status st) } {_message = "Reds Turn!"}
                    -- else 
                    -- st {_blackPieces  = replace start end (_blackPieces st),
                    --       _status = change (_status st) } {_message = "Reds Turn!"}
                    | otherwise = st {_message = "Illegal simple!!"}
                  where 
                    change Red = Black 
                    change Black = Red



apply_jump :: Move -> GameState -> GameState
apply_jump [] st = changeplayer st
apply_jump (start:(next:rest)) st
                    | _status st == Red && member start (_redKings st)
                              = apply_jump (next:rest)
                                (st{_blackKings = remove (jumped start next) (_blackKings st)
                                  ,_blackPieces = remove (jumped start next) (_blackPieces st)
                                  ,_redKings = next:(remove start (_redKings st))
                                  ,_message = ""})
                    | _status st == Black && member start (_blackKings st)
                                = apply_jump (next:rest)
                                  (st{_redKings = remove (jumped start next) (_redKings st)
                                    ,_redPieces = remove (jumped start next) (_redPieces st)
                                    ,_blackKings = next:(remove start (_blackKings st))
                                    ,_message = ""})
                    | _status st == Red && member start (_redPieces st)
                                  = apply_jump (next:rest)
                                    (st{_blackKings = remove (jumped start next) (_blackKings st)
                                      ,_blackPieces = remove (jumped start next) (_blackPieces st)
                                      ,_redPieces = next:(remove start (_redPieces st))
                                    ,_message = ""})
                    | _status st == Black && member start (_blackPieces st)
                                  = apply_jump (next:rest)
                                    (st{_redKings = remove (jumped start next) (_redKings st)
                                      ,_redPieces = remove (jumped start next) (_redPieces st)
                                      ,_blackPieces = next:(remove start (_blackPieces st))
                                      ,_message = ""})
                    | otherwise = st {_message = "Illegal jump !!"}
                  where 
                    jumped (x,y) (x',y') = ((x+x') `div` 2, (y+y') `div` 2)
apply_jump _ st = changeplayer st


