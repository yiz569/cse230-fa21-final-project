module Main where

import qualified Model.Block as Block
import qualified Model.Board as Board
import System.Exit
import Test.HUnit

testBlockRight = TestCase (assertEqual "blockRight" (Block.Pos 1 0) (Block.right (Block.Pos 0 0)))

testBlockRight_Edge = TestCase (assertEqual "blockRightEdge" (Block.Pos 3 0) (Block.right (Block.Pos 3 0)))

testBlockLeft = TestCase (assertEqual "blockLeft" (Block.Pos 2 0) (Block.left (Block.Pos 3 0)))

testBlockLeft_Edge = TestCase (assertEqual "blockLeftEdge" (Block.Pos 0 0) (Block.left (Block.Pos 0 0)))

testBlockUp = TestCase (assertEqual "blockUp" (Block.Pos 0 3) (Block.up (Block.Pos 0 4)))

testBlockUp_Edge = TestCase (assertEqual "blockUpEdge" (Block.Pos 0 0) (Block.up (Block.Pos 0 0)))

testBlockDown = TestCase (assertEqual "blockDown" (Block.Pos 0 4) (Block.down (Block.Pos 0 4)))

testBlockDown_Edge = TestCase (assertEqual "blockDownEdge" (Block.Pos 0 1) (Block.down (Block.Pos 0 0)))

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
            testBlockLeft,
            testBlockLeft_Edge,
            testBlockUp,
            testBlockUp_Edge,
            testBlockDown,
            testBlockDown_Edge,
            testFinished_True,
            testFinished_False
          ]
      )
  putStrLn "\nDone Testing"
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
  exitSuccess