module Model.Board where

import Model.Block

type Board = [Block]

(!) :: Board -> Pos -> Maybe Block
[] ! _ = Nothing
(b:bs) ! pos
  | covers b pos = Just b
  | otherwise = bs ! pos

blockRight :: Board -> Block -> Board
blockRight board block = if canRight board block then right board block else board 
  where
    canRight board (Target _ _ tx ty) = 
      if tx >= 2 then False else
        case board ! (Pos (tx+2) ty) of
          Just _ -> False
          _ -> case board ! (Pos (tx+2) (ty+1)) of
            Just _ -> False
            _ -> True
    canRight board (VDouble _ _ tx ty) = 
      if tx >= 3 then False else
        case board ! (Pos (tx+1) ty) of
          Just _ -> False
          _ -> case board ! (Pos (tx+1) (ty+1)) of
            Just _ -> False
            _ -> True
    canRight board (HDouble _ _ tx ty) = 
      if tx >= 2 then False else
        case board ! (Pos (tx+2) ty) of
          Just _ -> False
          _ -> True
    canRight board (Single _ _ tx ty) = 
      if tx >= 3 then False else
        case board ! (Pos (tx+1) ty) of
          Just _ -> False
          _ -> True
    right board block = go board block
      where
        go [] _ = []
        go (b:bs) block 
          | b /= block = b : (go bs block)
          | otherwise = b { posX = (posX b) + 1 } : bs

blockLeft :: Board -> Block -> Board
blockLeft board block = if canLeft board block then left board block else board 
  where
    canLeft board (Target _ _ tx ty) = 
      if tx <= 0 then False else
        case board ! (Pos (tx-1) ty) of
          Just _ -> False
          _ -> case board ! (Pos (tx-1) (ty+1)) of
            Just _ -> False
            _ -> True
    canLeft board (VDouble _ _ tx ty) = 
      if tx <= 0 then False else
        case board ! (Pos (tx-1) ty) of
          Just _ -> False
          _ -> case board ! (Pos (tx-1) (ty+1)) of
            Just _ -> False
            _ -> True
    canLeft board (HDouble _ _ tx ty) = 
      if tx <= 0 then False else
        case board ! (Pos (tx-1) ty) of
          Just _ -> False
          _ -> True
    canLeft board (Single _ _ tx ty) = 
      if tx <= 0 then False else
        case board ! (Pos (tx-1) ty) of
          Just _ -> False
          _ -> True
    left board block = go board block
      where
        go [] _ = []
        go (b:bs) block 
          | b /= block = b : (go bs block)
          | otherwise = b { posX = (posX b) - 1 } : bs

blockUp :: Board -> Block -> Board
blockUp board block = if canUp board block then up board block else board 
  where
    canUp board (Target _ _ tx ty) = 
      if ty <= 0 then False else
        case board ! (Pos tx (ty-1)) of
          Just _ -> False
          _ -> case board ! (Pos (tx+1) (ty-1)) of
            Just _ -> False
            _ -> True
    canUp board (VDouble _ _ tx ty) = 
      if ty <= 0 then False else
        case board ! (Pos tx (ty-1)) of
          Just _ -> False
          _ -> True
    canUp board (HDouble _ _ tx ty) = 
      if ty <= 0 then False else
        case board ! (Pos tx (ty-1)) of
          Just _ -> False
          _ -> case board ! (Pos (tx+1) (ty-1)) of
            Just _ -> False
            _ -> True
    canUp board (Single _ _ tx ty) = 
      if ty <= 0 then False else
        case board ! (Pos tx (ty-1)) of
          Just _ -> False
          _ -> True
    up board block = go board block
      where
        go [] _ = []
        go (b:bs) block 
          | b /= block = b : (go bs block)
          | otherwise = b { posY = (posY b) - 1 } : bs

blockDown :: Board -> Block -> Board
blockDown board block = if canDown board block then down board block else board 
  where
    canDown board (Target _ _ tx ty) = 
      if ty >= 3 then False else
        case board ! (Pos tx (ty+2)) of
          Just _ -> False
          _ -> case board ! (Pos (tx+1) (ty+2)) of
            Just _ -> False
            _ -> True
    canDown board (VDouble _ _ tx ty) = 
      if ty >= 3 then False else
        case board ! (Pos tx (ty+2)) of
          Just _ -> False
          _ -> True
    canDown board (HDouble _ _ tx ty) = 
      if ty >= 4 then False else
        case board ! (Pos tx (ty+1)) of
          Just _ -> False
          _ -> case board ! (Pos (tx+1) (ty+1)) of
            Just _ -> False
            _ -> True
    canDown board (Single _ _ tx ty) = 
      if ty >= 4 then False else
        case board ! (Pos tx (ty+1)) of
          Just _ -> False
          _ -> True
    down board block = go board block
      where
        go [] _ = []
        go (b:bs) block 
          | b /= block = b : (go bs block)
          | otherwise = b { posY = (posY b) + 1 } : bs

finished :: Board -> Bool
finished [] = False
finished (b:bs) =
  case b of
    Target _ _ x y ->
      if x == 1 && y == 3
      then
        True
      else
        False
    _ -> finished bs

