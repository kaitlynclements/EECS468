{-
Author: Kaitlyn Clements
KUID: 3072622
Date: 12/05/2023
Inputs: rows/num of stars
Outputs: rows and stars left
Description: Playing game of NIM
File: EECS 468 Assignment 9- Screen print of playing nim one time. 
-}

import Data.Char (isDigit, digitToInt) -- combined import statements

-- Define the type for the game board
type Board = [Int]

-- Define the initial game board
initial :: Board
initial = [5, 4, 3, 2, 1]

-- Function to display the current state of the board
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn $ unlines $ zipWith (curry (\ (row, stars) -> show row ++ ": " ++ replicate stars '*'))[1 .. ] board -- VSCode suggested this change be made to display the board correctly using zipWith
    putStrLn ""  -- Add a newline for clarity

-- Checking if the game is finished
isGameOver :: Board -> Bool
isGameOver = all (== 0)

-- Checking if a move is valid
isValidMove :: Board -> Int -> Int -> Bool
isValidMove board row stars = row >= 1 && row <= length board && stars >= 1 && stars <= board !! (row - 1)

-- Update the board after a valid move
updateBoard :: Board -> Int -> Int -> Board
updateBoard board row stars = take (row - 1) board ++ [max 0 (board !! (row - 1) - stars)] ++ drop row board

-- Recursive main game loop
play :: Board -> Int -> IO ()
play board player = do
    displayBoard board -- Display the board

    if isGameOver board -- Check if the game is finished
        then putStrLn $ "Player " ++ show (3 - player) ++ " wins!"  -- Other player wins
        else do
            putStrLn $ "Player " ++ show player
            putStr "Enter a row number: "
            row <- getLine -- taking user's row
            putStr "Stars to remove: "
            stars <- getLine -- taking user's number of stars
            -- Otherwise prompting current player for another move

            -- Check if the input is valid, otherwise display an error
            if all isDigit row && all isDigit stars
                then do
                    let rowNumber = read row
                        starsToRemove = read stars

                    if isValidMove board rowNumber starsToRemove -- if valid move
                        then play (updateBoard board rowNumber starsToRemove) (3 - player)  -- update board, Switch player
                        else do
                            putStrLn "ERROR: Invalid move"
                            play board player
                else do
                    putStrLn "ERROR: Invalid input"
                    play board player

-- Main to call nim
main :: IO()
main = nim

-- Play the game
nim :: IO ()
nim = play initial 1 -- starts the game with the initial board and player 1
