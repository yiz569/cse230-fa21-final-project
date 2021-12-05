module View (view) where

import Brick
import Brick.Widgets.Border (borderWithLabel, border, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, ascii)
import Text.Printf (printf)

import Model
import Model.Block as Block
import Model.Board as Board
import Graphics.Vty hiding (dim)

view :: PlayState -> [Widget String]
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vBox [ mkRow s row | row <- [0..4] ]

header :: PlayState -> String
header _ = printf "klotski"

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hBox [ mkCell s row col | col <- [0..3] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s y x
  | Model.isCurr s (Block.Pos x y) = withCursor (withBorderStyle ascii raw)
  | otherwise = withBorderStyle ascii raw
  where
    raw = case (Model.board s) Board.! (Block.Pos x y) of
      Nothing -> emptyBlock
      Just b -> case b of
        t@(Block.Target _ _ _ _) -> renderTarget t x y
        (Single _ _ _ _) -> single
        h@(HDouble _ _ _ _) -> renderHDouble h x y
        v@(VDouble _ _ _ _) -> renderVDouble v x y

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

renderTarget :: Block -> Int -> Int -> Widget n
renderTarget (Target _ _ tx ty) x y
  | tx == x && ty == y = targetTopLeft
  | tx == x && ty < y = targetBottomLeft
  | tx < x && ty == y = targetTopRight
  | otherwise = targetBottomRight
renderTarget _ _ _ = emptyBlock

renderHDouble :: Block -> Int -> Int -> Widget n
renderHDouble (HDouble _ _ hx hy) x y
  | hx == x && hy == y = hDoubleLeft
  | otherwise = hDoubleRight
renderHDouble _ _ _ = emptyBlock

renderVDouble :: Block -> Int -> Int -> Widget n
renderVDouble (VDouble _ _ hx hy) x y
  | hx == x && hy == y = vDoubleTop
  | otherwise = vDoubleBottom
renderVDouble _ _ _ = emptyBlock

----------------------------------- Basic Blocks ---------------------------------
block :: Widget n
block = vBox (replicate 5 row)

row :: Widget n
row = str "            "

col :: Widget n
col = vBox (replicate 5 (str " "))

emptyBlock :: Widget n
emptyBlock = (block <+> col <+> col) <=> (row <+> (str " ")) <=> (row <+> (str " "))

single, vDoubleTop, vDoubleBottom, hDoubleLeft, hDoubleRight :: Widget n
single = vLimit 7 $ hLimit 14 $ border block
vDoubleTop = vLimit 7 $ hLimit 14 $
                hBorder <=> (vBorder <+> block <+> vBorder)
vDoubleBottom = vLimit 7 $ hLimit 14 $
                  (vBorder <+> block <+> vBorder) <=> hBorder
hDoubleLeft = vLimit 7 $ hLimit 14 $ 
                vBorder <+> (hBorder <=> block <=> hBorder)
hDoubleRight = vLimit 7 $ hLimit 14 $ 
                (hBorder <=> block  <=> hBorder) <+> vBorder

targetTopLeft, targetTopRight, targetBottomLeft, targetBottomRight :: Widget n
targetTopLeft = vLimit 7 $ hLimit 14 $ joinBorders $
                  hBorder <=> (vBorder <+> block <+> col)
targetTopRight = vLimit 7 $ hLimit 14 $ joinBorders $
                  hBorder <=> (block <+> col <+> vBorder)
targetBottomLeft = vLimit 7 $ hLimit 14 $ joinBorders $
                    (vBorder <+> block <+> col) <=> hBorder
targetBottomRight = vLimit 7 $ hLimit 14 $ joinBorders $
                      (block <+> col <+> vBorder) <=> hBorder
-----------------------------------------------------------------------------------