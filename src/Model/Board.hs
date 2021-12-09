module Model.Board where

import Model.Block

type Board = [Block]

(!) :: Board -> Pos -> Maybe Block
[] ! _ = Nothing
(b : bs) ! pos
  | covers b pos = Just b
  | otherwise = bs ! pos

data MoveResult = MoveResult
  { board :: Board,
    step :: Int
  }

blockRight :: Board -> Block -> MoveResult
blockRight board block = if canRight board block then MoveResult (right board block) 1 else MoveResult board 0
  where
    canRight board (Target _ _ tx ty) =
      tx < 2
        && ( case board ! Pos (tx + 2) ty of
               Just _ -> False
               _ -> case board ! Pos (tx + 2) (ty + 1) of
                 Just _ -> False
                 _ -> True
           )
    canRight board (VDouble _ _ tx ty) =
      tx < 3
        && ( case board ! Pos (tx + 1) ty of
               Just _ -> False
               _ -> case board ! Pos (tx + 1) (ty + 1) of
                 Just _ -> False
                 _ -> True
           )
    canRight board (HDouble _ _ tx ty) =
      tx < 2
        && ( case board ! Pos (tx + 2) ty of
               Just _ -> False
               _ -> True
           )
    canRight board (Single _ _ tx ty) =
      tx < 3
        && ( case board ! Pos (tx + 1) ty of
               Just _ -> False
               _ -> True
           )
    right board block = go board block
      where
        go [] _ = []
        go (b : bs) block
          | b /= block = b : go bs block
          | otherwise = b {posX = posX b + 1} : bs

blockLeft :: Board -> Block -> MoveResult
blockLeft board block = if canLeft board block then MoveResult (left board block) 1 else MoveResult board 0
  where
    canLeft board (Target _ _ tx ty) =
      tx > 0
        && ( case board ! Pos (tx -1) ty of
               Just _ -> False
               _ -> case board ! Pos (tx -1) (ty + 1) of
                 Just _ -> False
                 _ -> True
           )
    canLeft board (VDouble _ _ tx ty) =
      tx > 0
        && ( case board ! Pos (tx -1) ty of
               Just _ -> False
               _ -> case board ! Pos (tx -1) (ty + 1) of
                 Just _ -> False
                 _ -> True
           )
    canLeft board (HDouble _ _ tx ty) =
      tx > 0
        && ( case board ! Pos (tx -1) ty of
               Just _ -> False
               _ -> True
           )
    canLeft board (Single _ _ tx ty) =
      tx > 0
        && ( case board ! Pos (tx -1) ty of
               Just _ -> False
               _ -> True
           )
    left board block = go board block
      where
        go [] _ = []
        go (b : bs) block
          | b /= block = b : go bs block
          | otherwise = b {posX = posX b - 1} : bs

blockUp :: Board -> Block -> MoveResult
blockUp board block = if canUp board block then MoveResult (up board block) 1 else MoveResult board 0
  where
    canUp board (Target _ _ tx ty) =
      ty > 0
        && ( case board ! Pos tx (ty -1) of
               Just _ -> False
               _ -> case board ! Pos (tx + 1) (ty -1) of
                 Just _ -> False
                 _ -> True
           )
    canUp board (VDouble _ _ tx ty) =
      ty > 0
        && ( case board ! Pos tx (ty -1) of
               Just _ -> False
               _ -> True
           )
    canUp board (HDouble _ _ tx ty) =
      ty > 0
        && ( case board ! Pos tx (ty -1) of
               Just _ -> False
               _ -> case board ! Pos (tx + 1) (ty -1) of
                 Just _ -> False
                 _ -> True
           )
    canUp board (Single _ _ tx ty) =
      ty > 0
        && ( case board ! Pos tx (ty -1) of
               Just _ -> False
               _ -> True
           )
    up board block = go board block
      where
        go [] _ = []
        go (b : bs) block
          | b /= block = b : go bs block
          | otherwise = b {posY = posY b - 1} : bs

blockDown :: Board -> Block -> MoveResult
blockDown board block = if canDown board block then MoveResult (down board block) 1 else MoveResult board 0
  where
    canDown board (Target _ _ tx ty) =
      ty < 3
        && ( case board ! Pos tx (ty + 2) of
               Just _ -> False
               _ -> case board ! Pos (tx + 1) (ty + 2) of
                 Just _ -> False
                 _ -> True
           )
    canDown board (VDouble _ _ tx ty) =
      ty < 3
        && ( case board ! Pos tx (ty + 2) of
               Just _ -> False
               _ -> True
           )
    canDown board (HDouble _ _ tx ty) =
      ty < 4
        && ( case board ! Pos tx (ty + 1) of
               Just _ -> False
               _ -> case board ! Pos (tx + 1) (ty + 1) of
                 Just _ -> False
                 _ -> True
           )
    canDown board (Single _ _ tx ty) =
      ty < 4
        && ( case board ! Pos tx (ty + 1) of
               Just _ -> False
               _ -> True
           )
    down board block = go board block
      where
        go [] _ = []
        go (b : bs) block
          | b /= block = b : go bs block
          | otherwise = b {posY = posY b + 1} : bs

finished :: Board -> Bool
finished [] = False
finished (b : bs) =
  case b of
    Target _ _ x y ->
      x == 1 && y == 3
    _ -> finished bs

encode :: Board -> Integer
encode b = sum $ f <$> b
  where
    f :: Block -> Integer
    f (Single _ _ x y) = 5 ^ (x * 5 + y)
    f (VDouble _ _ x y) = 2 * 5 ^ (x * 5 + y) + 2 * 5 ^ (x * 5 + y + 1)
    f (HDouble _ _ x y) = 3 * 5 ^ (x * 5 + y) + 2 * 5 ^ (x * 5 + 5 + y)
    f (Target _ _ x y) = 4 * (5 ^ (x * 5 + y) + 5 ^ (x * 5 + y + 1) + 5 ^ (x * 5 + 5 + y) + 5 ^ (x * 5 + 6 + y))
