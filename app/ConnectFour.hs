module ConnectFour where

import BoardHelper 

import Data.List
import Data.Array
import Data.Char
import Data.Maybe

data Chip = Red | Yellow
    deriving (Eq, Show)
-- get opposing color
opp :: Chip -> Chip
opp Red = Yellow
opp Yellow = Red
    
data Slot = Filled Chip | Empty
    deriving (Eq, Show)

type Board = RectArray Slot

type Move  = (Chip, Int) -- color of piece with column dropped into




emptyBoard :: Int -> Int -> Board
emptyBoard w h = array ((1,1),(w,h)) [((x,y),Empty) | x <- [1..w], y <- [1..h]]



-- dropping into a filled column doesn't change the board
-- (piece falls out lol)
dropPiece :: Board -> Move -> Board
dropPiece board (chip,col)
    | validMove = board//[((col,targetheight),Filled chip)]
    | otherwise = board
    where targetcolumn = [p | ((x, y), p) <- assocs board, x == col]
          targetheight = length $ (takeWhile (==Empty)) targetcolumn
          
          validMove = targetheight > 0 -- column is not full


-- returns Nothing if no winner is found, or Just (winner's color) if one is
checkWinner :: Board -> Maybe Chip
checkWinner board
    | any (all (==Filled Red)) (getLines board) = Just Red
    | any (all (==Filled Yellow)) (getLines board) = Just Yellow
    | otherwise = Nothing


-- holds state of board and current player
-- newtype so that we can define a Show that doesn't use the default Array implementaton
newtype GameState = GameState (Board, Chip)

instance Show GameState where
    show (GameState (board, player)) = (show player) ++ "'s Turn\n" ++ boardString ++ colNumbers
    
        where slotName s
                | s == Filled Red = 'R'
                | s == Filled Yellow = 'Y'
                | otherwise = ' '
              
              boardString = unlines $ map (intersperse '|' . map slotName) (arrayRows board)
              colNumbers = (intersperse '|' "1234567")
     


-- given a column, drops the corresponding color and switches players
-- if the move is invalid, keep players the same
makeMove :: GameState -> Int -> GameState
makeMove (GameState (board, player)) col 
    | validMove = GameState (dropPiece board (player, col), opp player)
    | otherwise = GameState (board, player)
    where validMove = (board /= (dropPiece board (player, col)))

-- pulls the board out of a gamestate
pullBoard :: GameState -> Board
pullBoard (GameState (board, _)) = board


     