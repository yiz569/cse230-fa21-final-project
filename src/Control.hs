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
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> Brick.continue (Model.init (Model.level s))
  T.VtyEvent (V.EvKey (V.KChar '1') _) -> Brick.continue (Model.init 0)
  T.VtyEvent (V.EvKey (V.KChar '2') _) -> Brick.continue (Model.init 1)
  T.VtyEvent (V.EvKey V.KRight _) -> case selected s of
    False -> Brick.continue (cursorRight s)
    _ -> exec (moveRight s)
  T.VtyEvent (V.EvKey V.KLeft _) -> case selected s of
    False -> Brick.continue (cursorLeft s)
    _ -> exec (moveLeft s)
  T.VtyEvent (V.EvKey V.KUp _) -> case selected s of
    False -> Brick.continue (cursorUp s)
    _ -> exec (moveUp s)
  T.VtyEvent (V.EvKey V.KDown _) -> case selected s of
    False -> Brick.continue (cursorDown s)
    _ -> exec (moveDown s)
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (select s)
  _ -> Brick.continue s

exec :: PlayState -> EventM n (Next PlayState)
exec s = case (Model.Board.finished (Model.board s)) of
  False -> continue s
  True -> halt s