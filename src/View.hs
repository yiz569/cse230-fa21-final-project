module View (view) where

import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Core as C
import Graphics.Vty hiding (dim)
import Model
import Model.Block as Block
import Model.Board as Board
import Text.Printf (printf)

view :: PlayState -> [Widget String]
view s = [view' s]

view' :: PlayState -> Widget String
view' s =
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vBox [mkRow s row | row <- [0 .. 4]] <=> prompt1 <=> prompt2 <=> prompt3

prompt1 :: Widget n
prompt1 = hLimit 56 $ joinBorders $ hBorderWithLabel (str "h: hint; b: back to game; r: restart level")

prompt2 :: Widget n
prompt2 = hLimit 56 $ joinBorders $ hBorderWithLabel (str "[1-7]: select levels; ↑↓←→: move;")

prompt3 :: Widget n
prompt3 = hLimit 56 $ joinBorders $ hBorderWithLabel (str "Enter: select/de-select")

header :: PlayState -> String
header s = printf "Klotski Level: %d  Step: %d" (level s + 1) (steps s)

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hBox [mkCell s row col | col <- [0 .. 3]]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s y x
  | Model.isCurr s (Block.Pos x y) && null (Model.solution s) =
    if Model.selected s then withCursor (withBorderStyle unicodeBold raw) else withCursor (withBorderStyle unicode raw)
  | otherwise = withBorderStyle unicode raw
  where
    boardToShow = case Model.solution s of
      [] -> Model.board s
      x : _ -> x
    raw = case boardToShow Board.! Block.Pos x y of
      Nothing -> emptyBlock
      Just b -> case b of
        t@Block.Target {} -> renderTarget t x y
        Single {} -> single
        h@HDouble {} -> renderHDouble h x y
        v@VDouble {} -> renderVDouble v x y

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
emptyBlock = (block <+> col <+> col) <=> (row <+> str " ") <=> (row <+> str " ")

single, vDoubleTop, vDoubleBottom, hDoubleLeft, hDoubleRight :: Widget n
single = vLimit 7 $ hLimit 14 $ border block
vDoubleTop = vLimit 7 $ hLimit 14 $ bottomlessBorder block
vDoubleBottom = vLimit 7 $ hLimit 14 $ toplessBorder block
hDoubleLeft = vLimit 7 $ hLimit 14 $ rightlessBorder block
hDoubleRight = vLimit 7 $ hLimit 14 $ leftlessBorder block

targetTopLeft, targetTopRight, targetBottomLeft, targetBottomRight :: Widget n
targetTopLeft = vLimit 7 $ hLimit 14 $ tlBorder block
targetTopRight = vLimit 7 $ hLimit 14 $ trBorder block
targetBottomLeft = vLimit 7 $ hLimit 14 $ blBorder block
targetBottomRight = vLimit 7 $ hLimit 14 $ brBorder block

-----------------------------------------------------------------------------------

customizedBorder :: Edges Bool -> Widget n -> Widget n
customizedBorder (Edges t b l r) mid =
  C.vBox
    [ C.hBox [tl_corner, top, tr_corner],
      C.hBox [left, mid, right],
      C.hBox [bl_corner, bottom, br_corner]
    ]
  where
    top = if t then hBorder else C.emptyWidget
    bottom = if b then hBorder else C.emptyWidget
    left = if l then vBorder else col
    right = if r then joinBorders vBorder else col
    tl_corner = if t && l then joinableBorder (Edges False True False True) else C.emptyWidget
    tr_corner = if t && r then joinableBorder (Edges False True True False) else C.emptyWidget
    bl_corner = if b && l then joinableBorder (Edges True False False True) else C.emptyWidget
    br_corner = if b && r then joinableBorder (Edges True False True False) else C.emptyWidget

toplessBorder :: Widget n -> Widget n
toplessBorder = customizedBorder (Edges False True True True)

bottomlessBorder :: Widget n -> Widget n
bottomlessBorder = customizedBorder (Edges True False True True)

leftlessBorder :: Widget n -> Widget n
leftlessBorder = customizedBorder (Edges True True False True)

rightlessBorder :: Widget n -> Widget n
rightlessBorder = customizedBorder (Edges True True True False)

tlBorder :: Widget n -> Widget n
tlBorder = customizedBorder (Edges True False True False)

trBorder :: Widget n -> Widget n
trBorder = customizedBorder (Edges True False False True)

blBorder :: Widget n -> Widget n
blBorder = customizedBorder (Edges False True True False)

brBorder :: Widget n -> Widget n
brBorder = customizedBorder (Edges False True False True)