module Control where

import Brick hiding (Result)
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Model
import Model.Board

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> Brick.continue (Model.init (Model.level s))
  T.VtyEvent (V.EvKey (V.KChar '1') _) -> Brick.continue (Model.init 0)
  T.VtyEvent (V.EvKey (V.KChar '2') _) -> Brick.continue (Model.init 1)
  T.VtyEvent (V.EvKey (V.KChar '3') _) -> Brick.continue (Model.init 2)
  T.VtyEvent (V.EvKey (V.KChar '4') _) -> Brick.continue (Model.init 3)
  T.VtyEvent (V.EvKey (V.KChar '5') _) -> Brick.continue (Model.init 4)
  T.VtyEvent (V.EvKey (V.KChar '6') _) -> Brick.continue (Model.init 5)
  T.VtyEvent (V.EvKey (V.KChar '7') _) -> Brick.continue (Model.init 6)
  T.VtyEvent (V.EvKey V.KRight _) -> if selected s then exec (wrapAction moveRight s) else Brick.continue (wrapAction cursorRight s)
  T.VtyEvent (V.EvKey V.KLeft _) -> if selected s then exec (wrapAction moveLeft s) else Brick.continue (wrapAction cursorLeft s)
  T.VtyEvent (V.EvKey V.KUp _) -> if selected s then exec (wrapAction moveUp s) else Brick.continue (wrapAction cursorUp s)
  T.VtyEvent (V.EvKey V.KDown _) -> if selected s then exec (wrapAction moveDown s) else Brick.continue (wrapAction cursorDown s)
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (wrapAction select s)
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> Brick.continue (hint s)
  T.VtyEvent (V.EvKey (V.KChar 'b') _) -> Brick.continue (backToGame s)
  _ -> Brick.continue s

exec :: PlayState -> EventM n (Next PlayState)
exec s = if Model.Board.finished (Model.board s) then halt s else continue s