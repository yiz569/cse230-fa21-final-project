# CSE230 Final Project: Klotski Game

Group Members: (github username)

- Yifeng Zhang (yiz569)
- Muyuan Chi (CVIllustrious)
- Xiaotian Shi (xshi1012)
- Yikuan Xia (kent0318)

# Introduction

In this project, we will implement an ancient sliding block puzzle game: Klotski.
In our game, we will provide a total of 15 levels where each has a different
starting layout for gamers to pass through. We will also record the number of
steps they take in each level. The fewer steps they take, the higher score they
will get for the game.

# How to play the game

A Klotski board contains ten blocks of different sizes to slide around on the
game board: one 2 * 2 square, four 1 * 1 squares, and five
1 * 2 rectangles. At any time, two of the twenty spots on the board are
always empty. The blocks cannot rotate or exit the board. The game ends when the
largest block reaches the bottom center before it can exit.

We will label the blocks from 1 to 10. Users can choose a block by clicking a
number and move that block either up, down, left or right into an adjacent empty
space using the up, down, left, and right arrow keys. Each time they move, the
step counter will increase by 1.

The goal of the game is to slide the blocks around to get the largest block
to the exit at the bottom center with as few steps as possible, so that it may
exit the playing area.

![](https://i.imgur.com/4b1M0yu.png)
