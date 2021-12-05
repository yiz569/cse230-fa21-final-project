module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, border, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, ascii)
import Text.Printf (printf)

import Model
import Graphics.Vty hiding (dim)

view :: PlayState -> [Widget String]
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      (single <+> emptyBlock <=> (hDoubleLeft <+> hDoubleRight))

header :: PlayState -> String
header _ = printf "klotski"

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