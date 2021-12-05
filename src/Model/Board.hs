module Model.Board where

import Model.Block

type Board = [Block]

(!) :: Board -> Pos -> Maybe Block
[] ! _ = Nothing
(b:bs) ! pos
  | covers b pos = Just b
  | otherwise = bs ! pos

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

