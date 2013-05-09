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
        gameLoop thePlayer m w
    else if line == "shoot" then
        if arrows p /= 0 then do
            thePlayer <- shootArrow p w
            gameLoop thePlayer m w
        else do
            putStrLn "You do not have any arrows left in your quiver."
            gameLoop p m w
    else do
        putStrLn "Sorry, that is not an option."
        gameLoop p m w
