module Model.Block where

data Pos = Pos
  { x :: Int
  , y :: Int 
  }
  deriving (Eq, Ord)

data Block = (HDouble | VDouble | Single
  { bName   :: String
  , bWidth  :: Int
  , bHeight :: Int
  , posX    :: Int
  , posY    :: Int
  }

covers :: Block -> Pos -> Bool
covers b pos =
  if (posX b) <= (x pos) && (posX b) + (bWidth b) > (x pos) &&
     (posY b) <= (y pos) && (posY b) + (bHeight b) > (y pos)
  then
    True
  else
    False

singleAtXy :: Int -> Int -> Block
singleAtXy = Block "single" 1 1

hDoubleAtXy :: Int -> Int -> Block
hDoubleAtXy = Block "hDouble" 2 1

vDoubleAtXy :: Int -> Int -> Block
vDoubleAtXy = Block "vDouble" 1 2

target :: Block
target = Block "target" 2 2 1 0
