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
  , curr      :: Block.Pos    -- cursor location
  , currBlock :: Maybe Block.Block -- current selected Block
  , selected  :: Bool
  }

init :: Int -> PlayState
init b = PS
  { board     = boards !! b
  , curr      = Block.Pos {Block.x = 0, Block.y = 0}
  , currBlock = Just (Block.vDoubleAtXy 0 0)
  , selected  = False
  }

isCurr :: PlayState -> Block.Pos -> Bool
isCurr s p = 
  case currBlock s of
    Just b -> Block.covers b p
    Nothing -> p == (curr s)

cursorRight :: PlayState -> PlayState
cursorRight s = case currBlock s of
  Nothing -> s { curr = newPos, currBlock = (board s) Board.! newPos}
    where
      newPos = Block.right (curr s)
  Just b -> s { curr = newPos, currBlock = (board s) Board.! newPos }
    where
      newPos = Block.right (Block.Pos ((Block.posX b) - 1 + (Block.bWidth b)) (Block.y (curr s)))

cursorLeft :: PlayState -> PlayState
cursorLeft s = case currBlock s of
  Nothing -> s { curr = newPos, currBlock = (board s) Board.! newPos}
    where
      newPos = Block.left (curr s)
  Just b -> s { curr = newPos, currBlock = (board s) Board.! newPos }
    where
      newPos = Block.left (Block.Pos (Block.posX b) (Block.y (curr s)))

cursorUp :: PlayState -> PlayState
cursorUp s = case currBlock s of
  Nothing -> s { curr = newPos, currBlock = (board s) Board.! newPos}
    where
      newPos = Block.up (curr s)
  Just b -> s { curr = newPos, currBlock = (board s) Board.! newPos }
    where
      newPos = Block.up (Block.Pos (Block.x (curr s)) (Block.posY b))

cursorDown :: PlayState -> PlayState
cursorDown s = case currBlock s of
  Nothing -> s { curr = newPos, currBlock = (board s) Board.! newPos}
    where
      newPos = Block.down (curr s)
  Just b -> s { curr = newPos, currBlock = (board s) Board.! newPos }
    where
      newPos = Block.down (Block.Pos (Block.x (curr s)) (Block.posY b - 1 + (Block.bHeight b)))

moveRight :: PlayState -> PlayState
moveRight s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = Board.blockRight (board s) b,
      currBlock = (board s) Board.! (curr s),
      selected = False
    }

moveLeft :: PlayState -> PlayState
moveLeft s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = Board.blockLeft (board s) b,
      currBlock = (board s) Board.! (curr s),
      selected = False
    }

moveUp :: PlayState -> PlayState
moveUp s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = Board.blockUp (board s) b,
      currBlock = (board s) Board.! (curr s),
      selected = False
    }

moveDown :: PlayState -> PlayState
moveDown s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = Board.blockDown (board s) b,
      currBlock = (board s) Board.! (curr s),
      selected = False
    }

select :: PlayState -> PlayState
select s = case currBlock s of
  Nothing -> s
  _ -> s { selected = not (selected s) }

boards :: [Board.Board]
boards = [simple]

simple :: Board.Board
simple = [Block.target,
          Block.vDoubleAtXy 0 0, Block.vDoubleAtXy 3 0, Block.vDoubleAtXy 0 2, Block.vDoubleAtXy 3 2,
          Block.hDoubleAtXy 1 2,
          Block.singleAtXy 1 3, Block.singleAtXy 2 3, Block.singleAtXy 0 4, Block.singleAtXy 3 4]
