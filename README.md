# CSE230 Final Project: Klotski Game

## Milestone 1: Registration and Proposal

Group Members: (github username)

- Yifeng Zhang (yiz569)
- Muyuan Chi (CVIllustrious)
- Xiaotian Shi (xshi1012)
- Yikuan Xia (kent0318)

### Introduction

In this project, we will implement an ancient sliding block puzzle game: Klotski.
In our game, we will provide a total of 15 levels where each has a different
starting layout. Players can choose a level to play with. We will also record
the number of steps they take in each level. The fewer steps they take, the
higher score they will get for the game.

### How to play the game

A Klotski game contains ten blocks of different sizes to slide around on a
5 * 4 game board: one 2 * 2 square, four 1 * 1 squares, and five
1 * 2 rectangles. At any time, two of 1 * 1 spots on the board are
always empty. The blocks cannot rotate or exit the board. The game ends when the
largest block reaches the bottom center before it can exit.

We will label the blocks from 0 to 9. Users can choose a block by clicking a
number and move that block either up, down, left or right into an adjacent empty
space using the up, down, left, and right arrow keys. Each time they move, the
step counter will increase by 1.

The goal of the game is to slide the blocks around to get the largest block
to the exit at the bottom center with as few steps as possible, so that it may
exit the playing area.

![One of the fifteen starting layout of Klotski game](https://i.imgur.com/4b1M0yu.png)

## Milestone 2: Updates

- What is the architecture of your application (the key components)?\
  We plan to implement our application using model-view-controller (MVC) design
  pattern. First of all, we will have a Main.hs file to run the game. Then we
  will divide the related program logic into three interconnected elements: Model,
  View and Control. The Model.hs will be the central component which handles data
  structures and manages the data, logic and rules of the application. This will 
  contain models for Block, Board, Selector, Score, and etc. The View.hs
  will be used to display the board state, score, and current selector position. 
  The Controller.hs will handle user keyboard inputs and tell the model or view 
  what to change. There should also be several levels of the game with different
  board setups available for the player to play.

- What challenges (if any) did you have so far and how did you solve them?\
  We originally planned to make a Klotski solver to automatically solve this game.
  However, it seems we have to focus on the development of this game rather than
  researching the solver. Besides, in our application, there are multiple targets
  that users can choose to move around. We are still trying to come up with a way
  to highlight the block players choose so they know which block they are actually
  moving around.

- Do you expect to meet your goals until the deadline?\
  We believe we can complete the development of the major functions and deliver a
  playale Klotski board game before the deadline. The key functions include showing
  the board, accepting user inputs, updating the board and score, and also checking
  whether players pass the game. However, as we mentioned in the previous section,
  we might not able to successfully deliver a automated Klotski solver but we will
  definitely try on that.

- If not, how will you modify your goals?\
  We also mentioned in the introduction that we will provide 15 levels of different
  starting layout. If time is limited, we consider to provide fewer levels but we
  will definitely deliver a playable Klotski game before the deadline.
