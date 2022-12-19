import ConnectFour

import Data.Maybe (isJust, fromJust) -- for pulling the possible winner
import Data.Char  (isDigit)          -- for checking whether a move is valid

-- given a gamestate, asks for a column and makes the corresponding move
handleGame :: GameState -> IO ()    
handleGame initial = do
    input <- getLine
    if isDigit (head input) then do -- if the user inputs a digit
        let updatedGame = makeMove initial (read input)
        print updatedGame
        
        let winner = checkWinner (pullBoard updatedGame)
        if isJust winner then do
            putStrLn $ "Congratulations!"
            putStrLn $ (show (fromJust winner)) ++ " has won!"
            return ()
        else handleGame updatedGame
        
    else do
        putStrLn $ "\n Enter a column from 1-7, please."
        handleGame initial
            

-- sets up an initial board and starts the game
main :: IO ()
main = do
    let e = emptyBoard 7 6
        initial = GameState (e, Red)
    
    print initial
    handleGame initial
    