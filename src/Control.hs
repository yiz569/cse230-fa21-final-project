module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  T.VtyEvent (V.EvKey V.KRight _) -> case selected s of
    False -> Brick.continue (cursorRight s)
    _ -> Brick.continue (moveRight s)
  T.VtyEvent (V.EvKey V.KLeft _) -> case selected s of
    False -> Brick.continue (cursorLeft s)
    _ -> Brick.continue (moveLeft s)
  T.VtyEvent (V.EvKey V.KUp _) -> case selected s of
    False -> Brick.continue (cursorUp s)
    _ -> Brick.continue (moveUp s)
  T.VtyEvent (V.EvKey V.KDown _) -> case selected s of
    False -> Brick.continue (cursorDown s)
    _ -> Brick.continue (moveDown s)
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (select s)
  _ -> Brick.continue s
