module CPUPlayer where

import ConnectFour

import Data.Array
import Data.List

import System.Random


-- returns a list of potential moves for the current player
-- this just gives a move for each column; it doesn't check whether those are valid
moveOptions :: GameState -> [Move]
moveOptions (GameState (board, color)) = [(color,x) | x <- [1..width]]
    where (width, height) = snd (bounds board)

-- returns a list of moves that actually update the GameState
validMoves :: GameState -> [Move]
validMoves game = filter (updatesGame) (moveOptions game)
    where updatesGame move = (makeMove game (snd move)) /= game

-- returns a list of moves that immediately win for the current player
winningMoves :: GameState -> [Move]
winningMoves game@(GameState (board, color)) = filter (isWinning) moves
    where  moves = moveOptions game
           isWinning move = checkWinner (dropPiece board move) == Just color
           
           (width, height) = snd (bounds $ pullBoard game)
           

-- returns a list of moves that let the opponent win next round
losingMoves :: GameState -> [Move]
losingMoves game = filter (isLosing) moves
    where moves = moveOptions game
          futureState (c, x) = makeMove game x 
          -- returns the game state after executing          
          -- a move is 'losing' if, after executing, a winning move exists
          isLosing move = (not . null) $ winningMoves (futureState move)
          
          (width, height) = snd (bounds $ pullBoard game)
          

-- given a random generator and a gamestate, decides a move
-- tries to win, then tries to not lose, then picks a random
decideMove :: StdGen -> GameState -> Move
decideMove rand game
    | winExists  = head (winningMoves game) -- pick the win if available
    | lossExists && canAvoid = head (survivingMoves) -- pick the move to not die if necessary and possible    
    | otherwise  = (validMoves game) !! (fst $ randomR (0, numMoves - 1) rand) -- otherwise, pick a random move
    where winExists  = (not . null) (winningMoves game)
          lossExists = (not . null) (losingMoves  game)
          canAvoid   = (length $ losingMoves game) < width -- ie, the losing moves don't cover every column
          
          survivingMoves = (validMoves game) \\ (losingMoves game)
          (width, height) = snd (bounds $ pullBoard game)
          
          numMoves = length (validMoves game)
          