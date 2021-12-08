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
  , steps     :: Int
  , finished  :: Bool
  , level     :: Int
  }

init :: Int -> PlayState
init b = PS
  { board     = boards !! b
  , curr      = Block.Pos {Block.x = 0, Block.y = 0}
  , currBlock = Just (Block.vDoubleAtXy 0 0)
  , selected  = False
  , steps = 0
  , finished = False
  , level = b
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
      board = (Board.board res),
      currBlock = (Board.board res) Board.! (curr s),
      selected = False,
      steps = (steps s) + (Board.step res),
      finished = Board.finished (Board.board res)
    }
      where
        res = Board.blockRight (board s) b

moveLeft :: PlayState -> PlayState
moveLeft s = if not (selected s) then s else
  case currBlock s of 
    Nothing -> s
    Just b -> s {
      board = (Board.board res),
      currBlock = (Board.board res) Board.! (curr s),
      selected = False,
      steps = (steps s) + (Board.step res),
      finished = Board.finished (Board.board res)
    }
      where
        res = Board.blockLeft (board s) b

moveUp :: PlayState -> PlayState
moveUp s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = (Board.board res),
      currBlock = (Board.board res) Board.! (curr s),
      selected = False,
      steps = (steps s) + (Board.step res),
      finished = Board.finished (Board.board res)
    }
      where
        res = Board.blockUp (board s) b

moveDown :: PlayState -> PlayState
moveDown s = if not (selected s) then s else
  case currBlock s of
    Nothing -> s
    Just b -> s {
      board = (Board.board res),
      currBlock = (Board.board res) Board.! (curr s),
      selected = False,
      steps = (steps s) + (Board.step res),
      finished = Board.finished (Board.board res)
    }
      where
        res = Board.blockDown (board s) b

select :: PlayState -> PlayState
select s = case currBlock s of
  Nothing -> s
  _ -> s { selected = not (selected s) }

checkWin :: PlayState -> PlayState
checkWin s = s {finished = Board.finished (board s)}

boards :: [Board.Board]
boards = [tutorial, simple]

simple :: Board.Board
simple = [Block.target,
          Block.vDoubleAtXy 0 0, Block.vDoubleAtXy 3 0, Block.vDoubleAtXy 0 2, Block.vDoubleAtXy 3 2,
          Block.hDoubleAtXy 1 2,
          Block.singleAtXy 1 3, Block.singleAtXy 2 3, Block.singleAtXy 0 4, Block.singleAtXy 3 4]

tutorial :: Board.Board
tutorial = [Block.target,
            Block.vDoubleAtXy 0 0]