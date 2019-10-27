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
   
















    


-- A function to collect a list of all posible moves for a red/black piece
simple_moves :: GameState -> [Move]
simple_moves s =
      if _status s == Red then (simpleKing (_redKings s)) ++ (simplePiece (_redPieces s)) --red_move s
      else if  _status s == Black then (simpleKing (_blackKings s)) ++ (simplePiece (_blackPieces s))
      else [[]]
    where 
      simplePiece xs = [[(x,y),(x',y')] | (x,y) <- xs,
                                          (x',y') <- [(x+1, y+dir), (x-1, y+dir)],
                                          emptyposition (x', y') s, onboard (x', y')]
                                          
      simpleKing xs = [[(x,y),(x',y')] | (x,y) <- xs,
                                         (x', y') <- [ (x+1,y+1), (x-1,y+1), (x+1,y-1),(x-1,y-1) ],
                                         emptyposition (x', y') s, onboard (x', y')]

      dir = case (_status s) of {Red -> -1; Black -> 1}
      emptyposition pos st = not (member pos (_blackKings st)) && not (member pos (_redKings st)) && not (member pos (_blackPieces st)) && not (member pos (_redPieces st))
      onboard pos = if (fst pos < 8 && fst pos >= 0) && (snd pos < 8 && snd pos >= 0)  then True else False

-- member :: GameState -> [Coord] -> Bool
member x [] = False
member x (a:as)
  | x == a = True
  | otherwise = member x as


replace start end [] = []
replace start end (a:as)
  | a == start = end:as
  | otherwise = a:(replace start end as)
   

  
    
jump_moves :: GameState -> [Move]
jump_moves st = 
      if _status st == Red then (jumpKing (_redKings st)) ++ (jumpPiece (_redPieces st)) --red_move s
      else if  _status st == Black then (jumpKing (_blackKings st)) ++ (jumpPiece (_blackPieces st))
      else [[]]
    where 
      jumpPiece xs =  [(x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y)]
      jumpPiece' start rem (x,y) =
                    [(x'',y''):ys
                    | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))],
                    not (member (x',y') rem) && opponent_occupied (x',y') st && (start==(x'',y'') && notoccupied (x'',y'') st && onboard (x'',y'')),
                    ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'')) ]


      jumpKing xs = [(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y)]
      jumpKing' start rem (x,y) =
                    [ (x'',y''):ys
                    | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                    , not (member (x',y') rem) && opponent_occupied (x',y') st && (start==(x'',y'') && notoccupied (x'',y'') st && onboard (x'',y''))
                    , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'')) ]

      notoccupied pos st = not (member pos (_blackKings st)) && not (member pos (_redKings st)) && not (member pos (_blackPieces st)) && not (member pos (_redPieces st))
      onboard pos = if (fst pos < 8 && fst pos >= 0) && (snd pos < 8 && snd pos >= 0)  then True else False
      opponent_occupied pos st = (member pos (_blackKings st)) || (member pos (_redKings st)) || (member pos (_blackPieces st)) || (member pos (_redPieces st))
      jump_over [] = [[]]
      jumpover z = z

-- data PieceType = RedKing|BlackKing|RedPiece|BlackPiece

moves st 
  | jumpmoves /= [] = jumpmoves
  | otherwise = simplemoves
    where
      simplemoves = simple_moves st
      jumpmoves = jump_moves st


-- apply_move' :: Move -> GameState -> GameState
-- apply_move' mv st | member mv (mover st) = snd(runSM mover st)
--                   | otherwise = st{_message = "Bad move bro"}

-- mover :: Move -> SM(GameState, ())
-- mover [end] = return ()
-- mover (m1:m2:m3) = 
--         do 
--             pt <- removepiece m1
--             _ <- removepiece (jump m1 m2)
--             _ <- addpiece pt m2
--             mover (m2:m1)


-- removepeice :: Coord -> SM (GameState PieceType)
-- removepiece xy = SM (\st -> piece_remover xy st)
--     where 
--         piece_remover :: Coord -> GameState -> (PieceType, GameState)
--         piece_remover xy st | member xy (_redKings st)
--                             = (RedKing, st {_redKings = remove xy 
--                                                             (_redKings st)})

-- addpiece :: PieceType -> Coord -> SM (GameState, ())
-- addpiece pt xy = SM(piece_adder xy)
--         where 
--             piece_adder :: PieceType -> Coord -> GameState -> ((),GameState)
--             piece_adder RedKing xy st = st{_redKings = xy:(_redKings st)}

-- data PieceType = RedKing|BlackKing|RedPiece|BlackPiece

applyMove :: Move -> GameState -> GameState
applyMove m s = case _status s of
    Red -> if m `elem` (moves s) then (apply_move m s) else setMessage $ set status Black s
    Black -> if m `elem` (moves s)  then  (apply_move m s) else setMessage $ set status Red s
    _ -> initialGameState



apply_move :: Move -> GameState -> GameState
apply_move [start, end] st
                    | member start (_redKings st) = 
                    st {_redKings = replace start end (_redKings st), 
                        _status = changeplayer (_status st) }
                    | member start (_blackKings st) =
                    st {_blackKings = replace start end (_blackKings st),
                        _status = changeplayer (_status st) }
                    | member start (_redPieces st) =
                    st {_redPieces = replace start end (_redPieces st),
                          _status = changeplayer (_status st) }
                    | member start (_blackPieces st) =
                    st {_blackPieces  = replace start end (_blackPieces st),
                          _status = changeplayer (_status st) }
                    | otherwise = st {_message = "Illegal Move!!!"}
                where 
                  changeplayer Red = Black
                  changeplayer Black = Red


-- apply_move :: GameState -> Move -> GameState
-- apply_move s m = case _status s of
--   Red -> if m `elem` (simple_moves s) then s{_redPieces = (last m):[x | x <- _redPieces s, not (head m == x)]} else s
--   Black -> if m `elem` (simple_moves s) then s{_blackPieces = (last m):[x | x <- _blackPieces s, not (head m == x)]} else s
--   _ -> initialGameState  
  


-- red_move :: GameState -> [Move]
-- red_move s = [[(x,y),(a,b)] | (x,y) <- _redPieces s,
--                               (a,b) <- [(x-1,y-1),(x+1,y-1)]]


-- black_move :: GameState -> [Move]
-- black_move s = [[(x,y),(a,b)]  |  (x,y) <- _blackPieces s, 
--                                   (a, b) <- [(x+1,y+1), (x-1,y+1)]]
