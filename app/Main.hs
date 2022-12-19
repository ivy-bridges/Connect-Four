import ConnectFour
import CPUPlayer


import Data.Maybe    (isJust, fromJust)              -- for pulling the possible winner
import Data.Char     (isDigit)                       -- for checking whether a move is valid
import System.Random

-- given a gamestate, asks for a column and makes the corresponding move, then hands game back to itself
handle2PGame :: GameState -> IO ()    
handle2PGame initial = do
    input <- getLine
    if isDigit (head input) then do
        -- if the user enters a digit, make the corresponding move and print the game state
        let updatedGame = makeMove initial (read input)
        print updatedGame
        
        -- checks for winner, then congratulates and ends game if one exists
        let winner = checkWinner (pullBoard updatedGame)
        if isJust winner then do
            putStrLn $ "Congratulations!"
            putStrLn $ (show (fromJust winner)) ++ " has won!"
            return ()
        -- otherwise, move to next move
        -- invalid moves will not update the current player,
        -- so this handles passing between turns as well as retaking the current one
        else handle2PGame updatedGame
        
    else do
        putStrLn $ "\n Enter a column from 1-7, please."
        handle2PGame initial
 

-- functions for playing against a computer 

-- lets player make a move, then updates generator and passes to cpu
playerTurn :: GameState -> IO ()
playerTurn game = do
    input <- getLine
    if isDigit (head input) then do
        let updatedGame = makeMove game (read input)
        print updatedGame
        
        -- checks for winner, then congratulates and ends game if one exists
        let winner = checkWinner (pullBoard updatedGame)
        if isJust winner then do
            putStrLn $ "Congratulations!"
            putStrLn $ (show (fromJust winner)) ++ " has won!"
            return ()
        
        -- checks for invalid moves (ones that don't update the board)
        -- alerts player and restarts turn
        else if updatedGame == game then do
            putStrLn $ "Invalid move."
            playerTurn game
        -- updates random generator and passes to cpu
        else do
            randGen <- newStdGen
            cpuTurn randGen updatedGame
            
    -- alerts player and restarts turn on invalid input
    else do
        putStrLn $ "\n Enter a column from 1-7, please."
        playerTurn game

-- takes a turn for the computer
cpuTurn :: StdGen -> GameState -> IO ()
cpuTurn randGen game = do
    -- finds cpu move and plays it
    let cpuMove = decideMove randGen game
        updatedGame = makeMove game (snd cpuMove) 
        
    -- prints cpu move as well as state of the game
    putStrLn $ "The Computer played to column " ++ show (snd cpuMove)
    print updatedGame
    
    -- checks for a winner, then ends game if one exists
    let winner = checkWinner (pullBoard updatedGame)
    if isJust winner then do
        putStrLn $ "Sorry!"
        putStrLn $ "The Computer has won..."
        return ()
    -- otherwise, pass back to the player
    else do
        playerTurn updatedGame
        
        

-- sets up an initial board and starts the game
-- plays with CPU or with two players depending on response
main :: IO ()
main = do
    let e = emptyBoard 7 6
        initial = GameState (e, Red)
    
    initialGen <- getStdGen
    
    putStrLn $ "Play against computer? (Y/N)"
    decision <- getLine
    
    if (head decision) == 'Y' then do
        print initial
        playerTurn initial
    else do
        print initial
        handle2PGame initial
    