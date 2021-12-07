module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View
import Control

import qualified Model.Board as Board

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <- customMain initialVty buildVty (Just chan) app (Model.init 0)
  if (Model.finished res) then
    putStrLn $ "Congrats! You finised game level: " ++ (show (Model.level res + 1)) ++ " in " ++ (show (Model.steps res)) ++ " steps"
  else 
    putStrLn "Game Over"

app :: App PlayState Tick String
app = App
  { appDraw         = view
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])    
  }
