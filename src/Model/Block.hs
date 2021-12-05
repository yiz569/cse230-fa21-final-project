module Model.Block where

data Pos = Pos
  { x :: Int
  , y :: Int 
  }
  deriving (Eq)

right :: Pos -> Pos
right (Pos x y)
  | x < 3 = Pos (x+1) y
  | otherwise = Pos x y

left :: Pos -> Pos
left (Pos x y)
  | x > 0 = Pos (x-1) y
  | otherwise = Pos x y

up :: Pos -> Pos
up (Pos x y)
  | y > 0 = Pos x (y-1)
  | otherwise = Pos x y

down :: Pos -> Pos
down (Pos x y)
  | y < 4 = Pos x (y+1)
  | otherwise = Pos x y

data Block = HDouble { bWidth  :: Int
  , bHeight :: Int
  , posX    :: Int
  , posY    :: Int
  }
  | VDouble { bWidth  :: Int
  , bHeight :: Int
  , posX    :: Int
  , posY    :: Int
  }
  | Single { bWidth  :: Int
  , bHeight :: Int
  , posX    :: Int
  , posY    :: Int
  }
  | Target
  { bWidth  :: Int
  , bHeight :: Int
  , posX    :: Int
  , posY    :: Int
  }
  deriving (Eq)

covers :: Block -> Pos -> Bool
covers b pos =
  if (posX b) <= (x pos) && (posX b) + (bWidth b) > (x pos) &&
     (posY b) <= (y pos) && (posY b) + (bHeight b) > (y pos)
  then
    True
  else
    False

singleAtXy :: Int -> Int -> Block
singleAtXy = Single 1 1

hDoubleAtXy :: Int -> Int -> Block
hDoubleAtXy = HDouble 2 1

vDoubleAtXy :: Int -> Int -> Block
vDoubleAtXy = VDouble 1 2

target :: Block
target = Target 2 2 1 0
