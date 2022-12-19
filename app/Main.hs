import ConnectFour
import CPUPlayer


import Data.Maybe    (isJust, fromJust)              -- for pulling the possible winner
import Data.Char     (isDigit)                       -- for checking whether a move is valid
import System.Random

-- given a gamestate, asks for a column and makes the corresponding move, then hands game back to itself
handle2PGame :: GameState -> IO ()    
handle2PGame initial = do
    input <- getLine
    if isDigit (head input) then do -- if the user inputs a digit
        let updatedGame = makeMove initial (read input)
        print updatedGame
        
        let winner = checkWinner (pullBoard updatedGame)
        if isJust winner then do
            putStrLn $ "Congratulations!"
            putStrLn $ (show (fromJust winner)) ++ " has won!"
            return ()
        else handle2PGame updatedGame
        
    else do
        putStrLn $ "\n Enter a column from 1-7, please."
        handle2PGame initial
 

-- functions for playing against a computer 
playerTurn :: GameState -> IO ()
playerTurn game = do
    input <- getLine
    if isDigit (head input) then do
        let updatedGame = makeMove game (read input)
        print updatedGame
        
        let winner = checkWinner (pullBoard updatedGame)
        if isJust winner then do
            putStrLn $ "Congratulations!"
            putStrLn $ (show (fromJust winner)) ++ " has won!"
            return ()
            
        else if updatedGame == game then do -- this happens if the move didn't change the board
            putStrLn $ "Invalid move."
            playerTurn game
        else do
            randGen <- newStdGen
            cpuTurn randGen updatedGame 
        
    else do
        putStrLn $ "\n Enter a column from 1-7, please."
        playerTurn game

-- takes a turn for the computer
cpuTurn :: StdGen -> GameState -> IO ()
cpuTurn randGen game = do
    let cpuMove = decideMove randGen game
        updatedGame = makeMove game (snd cpuMove) 
        
    
    putStrLn $ "The Computer played to column " ++ show (snd cpuMove)
    print updatedGame
        
    let winner = checkWinner (pullBoard updatedGame)
    if isJust winner then do
        putStrLn $ "Sorry!"
        putStrLn $ "The Computer has won..."
        return ()
    else do
        playerTurn updatedGame
        
        

-- sets up an initial board and starts the game
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
    