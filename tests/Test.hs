module Main where

import qualified Model.Block as Block
import qualified Model.Board as Board
import System.Exit
import Test.HUnit

testBlockRight = TestCase (assertEqual "blockRight" (Block.Pos 1 0) (Block.right (Block.Pos 0 0)))

testBlockRight_Edge = TestCase (assertEqual "blockRightEdge" (Block.Pos 3 0) (Block.right (Block.Pos 3 0)))

testMoveRight_Blocked = TestCase (assertEqual "blockRightBlocked" (Board.MoveResult [Block.target, Block.vDoubleAtXy 3 0] 0) (Board.blockRight [Block.target, Block.vDoubleAtXy 3 0] Block.target))

testBlockLeft = TestCase (assertEqual "blockLeft" (Block.Pos 2 0) (Block.left (Block.Pos 3 0)))

testBlockLeft_Edge = TestCase (assertEqual "blockLeftEdge" (Block.Pos 0 0) (Block.left (Block.Pos 0 0)))

testMoveLeft_Blocked = TestCase (assertEqual "blockLeftBlocked" (Board.MoveResult [Block.target, Block.singleAtXy 0 0] 0) (Board.blockLeft [Block.target, Block.singleAtXy 0 0] Block.target))

testBlockUp = TestCase (assertEqual "blockUp" (Block.Pos 0 3) (Block.up (Block.Pos 0 4)))

testBlockUp_Edge = TestCase (assertEqual "blockUpEdge" (Block.Pos 0 0) (Block.up (Block.Pos 0 0)))

testMoveUP_Blocked = TestCase (assertEqual "blockUpBlocked" (Board.MoveResult [Block.target, Block.vDoubleAtXy 2 2] 0) (Board.blockUp [Block.target, Block.vDoubleAtXy 2 2] (Block.vDoubleAtXy 2 2)))

testBlockDown = TestCase (assertEqual "blockDown" (Block.Pos 0 4) (Block.down (Block.Pos 0 4)))

testBlockDown_Edge = TestCase (assertEqual "blockDownEdge" (Block.Pos 0 1) (Block.down (Block.Pos 0 0)))

testMoveDown_Blocked = TestCase (assertEqual "blockDownBlocked" (Board.MoveResult [Block.target, Block.singleAtXy 2 2] 0) (Board.blockDown [Block.target, Block.singleAtXy 2 2] Block.target))

testFinished_True = TestCase (assertEqual "finished_true" (Board.finished [Block.Target 2 2 1 3]) True)

testFinished_False = TestCase (assertEqual "finished_false" (Board.finished [Block.Target 2 2 1 2]) False)

main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  counts <-
    runTestTT
      ( test
          [ testBlockRight,
            testBlockRight_Edge,
            testMoveRight_Blocked,
            testBlockLeft,
            testBlockLeft_Edge,
            testMoveLeft_Blocked,
            testBlockUp,
            testBlockUp_Edge,
            testMoveUP_Blocked,
            testBlockDown,
            testBlockDown_Edge,
            testMoveDown_Blocked,
            testFinished_True,
            testFinished_False
          ]
      )
  putStrLn "\nDone Testing"
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
  exitSuccess