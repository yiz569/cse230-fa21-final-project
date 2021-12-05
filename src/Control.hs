module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.halt s
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (cursorRight s)
  T.VtyEvent (V.EvKey V.KLeft _) -> Brick.continue (cursorLeft s)
  T.VtyEvent (V.EvKey V.KUp _) -> Brick.continue (cursorUp s)
  T.VtyEvent (V.EvKey V.KDown _) -> Brick.continue (cursorDown s)
  _ -> Brick.continue s
