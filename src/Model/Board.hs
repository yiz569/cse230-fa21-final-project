module Model.Board where

import Model.Block

type Board = [Block]

(!) :: Board -> Pos -> Maybe Block
[] ! _ = Nothing
(b:bs) ! pos
  | covers b pos = Just b
  | otherwise = bs ! pos

empty :: Board
empty = []

finished :: Board -> Bool
finished [] = False
finished (b:bs)
  | bName b == "target" =
      if posX b == 1 && posY b == 3
      then
        True
      else
        False
  | otherwise = finished bs

