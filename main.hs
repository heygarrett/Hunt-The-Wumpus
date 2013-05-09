module Main 
    ( main
    ) where

import System.Random
import Map
import Player

main :: IO ()
main = do
    putStrLn "How many rooms would you like the map to have? It must be an even number!"
    line <- fmap read getLine
    let theMap = generateMap line (div line 2)
    let thePlayer = Player 1 line 3
    loc <- randomRIO (2, line)
    let theWumpus = Wumpus loc
    gameLoop thePlayer theMap theWumpus

gameLoop :: Player -> Map -> Wumpus -> IO ()
gameLoop p m w = do
    checkForWumpus p m w
    -- print $ show p
    putStrLn "Would you like to move or shoot?"
    line <- getLine
    if line == "move" then do
        thePlayer <- movePlayer p m
        if location thePlayer == wloc w then
            gameOver False
        else
            gameLoop thePlayer m w
    else if line == "shoot" then
        if arrows p /= 0 then do
            (thePlayer, b) <- shootArrow p m w
            if b then
                gameOver True
            else
                gameLoop thePlayer m w
        else do
            putStrLn "You do not have any arrows left in your quiver."
            gameOver False
    else do
        putStrLn "Sorry, that is not an option."
        gameLoop p m w

gameOver :: Bool -> IO ()
gameOver b =
    if b then do
        putStrLn "There is a moaning sound in the next room before you hear something large crash to the ground. You killed the Wumpus! You win!"
        putStrLn "Would you like to play again? (yes/no)"
        line <- getLine
        if line == "yes" then
            main
        else if line == "no" then
            putStrLn "Thanks for playing!"
        else
            putStrLn "Lolwut."
    else
        putStrLn "You walked into room with the wumpus! He ate you. Oops."
