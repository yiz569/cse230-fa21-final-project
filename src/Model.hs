module Model where

import qualified Model.Block as Block
import qualified Model.Board as Board

data Tick = Tick

data State
  = Intro
  | Play PlayState
  | Outro

data PlayState = PS
  { board     :: Board.Board  -- current board
  , selector  :: Block.Pos    -- cursor location
  , selected  :: Maybe Block.Block -- current selected Block
  }

init :: Board.Board -> PlayState
init b = PS
  { board     = b
  , selector  = Block.Pos {Block.x = 0, Block.y = 0}
  , selected  = Nothing
  }
