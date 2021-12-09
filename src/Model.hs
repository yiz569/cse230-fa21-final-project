module Model where

-- import Data.Set (Set, notMember, union, fromList)
-- import qualified Data.Set as Set

import Data.Map (Map, insert, notMember, (!))
import qualified Data.Map as Map
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Model.Block as Block
import Model.Board (blockDown, blockLeft, blockRight, blockUp)
import qualified Model.Board as Board

data Tick = Tick

data State
  = Intro
  | Play PlayState
  | Outro

data PlayState = PS
  { board :: Board.Board, -- current board
    curr :: Block.Pos, -- cursor location
    selected :: Bool,
    steps :: Int,
    finished :: Bool,
    level :: Int,
    solution :: [Board.Board]
  }

init :: Int -> PlayState
init b =
  PS
    { board = boards !! b,
      curr = Block.Pos {Block.x = 0, Block.y = 0},
      selected = False,
      steps = 0,
      finished = False,
      level = b,
      solution = []
    }

currBlock :: PlayState -> Maybe Block.Block
currBlock s = board s Board.! curr s

isCurr :: PlayState -> Block.Pos -> Bool
isCurr s p =
  case currBlock s of
    Just b -> Block.covers b p
    Nothing -> p == curr s

cursorRight :: PlayState -> PlayState
cursorRight s = case currBlock s of
  Nothing -> s {curr = newPos}
    where
      newPos = Block.right (curr s)
  Just b -> s {curr = newPos}
    where
      newPos = Block.right (Block.Pos (Block.posX b - 1 + Block.bWidth b) (Block.y (curr s)))

cursorLeft :: PlayState -> PlayState
cursorLeft s = case currBlock s of
  Nothing -> s {curr = newPos}
    where
      newPos = Block.left (curr s)
  Just b -> s {curr = newPos}
    where
      newPos = Block.left (Block.Pos (Block.posX b) (Block.y (curr s)))

cursorUp :: PlayState -> PlayState
cursorUp s = case currBlock s of
  Nothing -> s {curr = newPos}
    where
      newPos = Block.up (curr s)
  Just b -> s {curr = newPos}
    where
      newPos = Block.up (Block.Pos (Block.x (curr s)) (Block.posY b))

cursorDown :: PlayState -> PlayState
cursorDown s = case currBlock s of
  Nothing -> s {curr = newPos}
    where
      newPos = Block.down (curr s)
  Just b -> s {curr = newPos}
    where
      newPos = Block.down (Block.Pos (Block.x (curr s)) (Block.posY b - 1 + Block.bHeight b))

moveRight :: PlayState -> PlayState
moveRight s =
  if not (selected s)
    then s
    else case currBlock s of
      Nothing -> s
      Just b ->
        s
          { board = Board.board res,
            selected = False,
            steps = steps s + Board.step res,
            finished = Board.finished (Board.board res)
          }
        where
          res = Board.blockRight (board s) b

moveLeft :: PlayState -> PlayState
moveLeft s =
  if not (selected s)
    then s
    else case currBlock s of
      Nothing -> s
      Just b ->
        s
          { board = Board.board res,
            selected = False,
            steps = steps s + Board.step res,
            finished = Board.finished (Board.board res)
          }
        where
          res = Board.blockLeft (board s) b

moveUp :: PlayState -> PlayState
moveUp s =
  if not (selected s)
    then s
    else case currBlock s of
      Nothing -> s
      Just b ->
        s
          { board = Board.board res,
            selected = False,
            steps = steps s + Board.step res,
            finished = Board.finished (Board.board res)
          }
        where
          res = Board.blockUp (board s) b

moveDown :: PlayState -> PlayState
moveDown s =
  if not (selected s)
    then s
    else case currBlock s of
      Nothing -> s
      Just b ->
        s
          { board = Board.board res,
            selected = False,
            steps = steps s + Board.step res,
            finished = Board.finished (Board.board res)
          }
        where
          res = Board.blockDown (board s) b

select :: PlayState -> PlayState
select s = case currBlock s of
  Nothing -> s
  _ -> s {selected = not (selected s)}

checkWin :: PlayState -> PlayState
checkWin s = s {finished = Board.finished (board s)}

boards :: [Board.Board]
boards = [level1_tutorial, level_2, level_3, level_4, level_5, level_6, level_7]

level1_tutorial :: Board.Board
level1_tutorial =
  [ Block.target,
    Block.vDoubleAtXy 0 0
  ]

level_2 :: Board.Board
level_2 =
  [ Block.target,
    Block.vDoubleAtXy 0 0,
    Block.vDoubleAtXy 3 0,
    Block.vDoubleAtXy 0 2,
    Block.vDoubleAtXy 3 2,
    Block.hDoubleAtXy 1 2,
    Block.singleAtXy 1 3,
    Block.singleAtXy 2 3,
    Block.singleAtXy 0 4,
    Block.singleAtXy 3 4
  ]

level_3 :: Board.Board
level_3 =
  [ Block.target,
    Block.vDoubleAtXy 0 0,
    Block.vDoubleAtXy 0 3,
    Block.vDoubleAtXy 3 3,
    Block.hDoubleAtXy 0 2,
    Block.hDoubleAtXy 1 3,
    Block.singleAtXy 3 0,
    Block.singleAtXy 3 1,
    Block.singleAtXy 3 2,
    Block.singleAtXy 2 2
  ]

level_4 :: Board.Board
level_4 =
  [ Block.target,
    Block.vDoubleAtXy 0 0,
    Block.vDoubleAtXy 3 0,
    Block.hDoubleAtXy 1 2,
    Block.hDoubleAtXy 1 3,
    Block.hDoubleAtXy 1 4,
    Block.singleAtXy 0 2,
    Block.singleAtXy 0 3,
    Block.singleAtXy 3 2,
    Block.singleAtXy 3 3
  ]

level_5 :: Board.Board
level_5 =
  [ Block.Target 2 2 0 0,
    Block.vDoubleAtXy 2 0,
    Block.vDoubleAtXy 3 0,
    Block.vDoubleAtXy 0 3,
    Block.vDoubleAtXy 1 3,
    Block.hDoubleAtXy 0 2,
    Block.singleAtXy 2 2,
    Block.singleAtXy 2 3,
    Block.singleAtXy 3 2,
    Block.singleAtXy 3 3
  ]

level_6 :: Board.Board
level_6 =
  [ Block.target,
    Block.vDoubleAtXy 0 0,
    Block.vDoubleAtXy 3 0,
    Block.vDoubleAtXy 1 2,
    Block.hDoubleAtXy 0 4,
    Block.hDoubleAtXy 2 4,
    Block.singleAtXy 0 2,
    Block.singleAtXy 0 3,
    Block.singleAtXy 3 2,
    Block.singleAtXy 3 3
  ]

level_7 :: Board.Board
level_7 =
  [ Block.target,
    Block.vDoubleAtXy 3 0,
    Block.hDoubleAtXy 0 2,
    Block.hDoubleAtXy 2 2,
    Block.hDoubleAtXy 0 3,
    Block.hDoubleAtXy 2 3,
    Block.singleAtXy 0 0,
    Block.singleAtXy 0 1,
    Block.singleAtXy 0 4,
    Block.singleAtXy 3 4
  ]

hint :: PlayState -> PlayState
hint s = case solution s of
  [] -> hint s {solution = bfs (Seq.singleton $ board s) (Map.singleton (Board.encode $ board s) (board s))}
  _ : xs -> s {solution = xs}

backToGame :: PlayState -> PlayState
backToGame s = s {solution = []}

wrapAction :: (PlayState -> PlayState) -> PlayState -> PlayState
wrapAction action s = case solution s of
  [] -> action s
  _ -> s

bfs :: Seq Board.Board -> Map Integer Board.Board -> [Board.Board]
bfs Empty _ = []
bfs (x :<| xs) seen
  | Board.finished x = reverse $ f x
  | otherwise = bfs newQueue newSeen
  where
    newBoards =
      filter ((`notMember` seen) . Board.encode) $
        Board.board
          <$> ([blockDown x, blockLeft x, blockUp x, blockRight x] <*> x)
    newSeen = foldr (\b m -> insert (Board.encode b) x m) seen newBoards
    newQueue = xs >< Seq.fromList newBoards
    f :: Board.Board -> [Board.Board]
    f b =
      if parent == b
        then [b]
        else b : f parent
      where
        parent = seen ! Board.encode b