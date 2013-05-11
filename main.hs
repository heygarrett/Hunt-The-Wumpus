module Main 
    ( main
    ) where

import System.IO
import System.Random
import Map
import Player

main :: IO ()
main = do
    putStrLn "Welcome to Hunt the Wumpus!"
    putStrLn "When you start the game, you will be placed in a map "
    putStrLn "with the number of rooms of your choosing. "
    putStrLn "Each room is connected to three other rooms via dark hallways, "
    putStrLn "so in any given room, there will be three directions: "
    putStrLn "right, left, and back. "
    putStrLn "You're job is to shoot the Wumpus with an arrow! "
    putStrLn "But be careful, if you walk into the room with the Wumpus, he'll eat you! "
    putStrLn "If you smell the Wumpus, it means he's in an adjacent room. "
    n <- numberRooms
    let theMap = generateMap n (div n 2)
    let thePlayer = Player 1 n 3
    loc <- randomRIO (2, n)
    let theWumpus = Wumpus loc 0
    gameLoop thePlayer theMap theWumpus

gameLoop :: Player -> Map -> Wumpus -> IO ()
gameLoop p m w = do
    result <- checkForWumpus p m w
    if result == 0 
        then do 
            putStrLn "There is a foul stench in the air..."
            gameAction p m w
        else if result == 1 
            then gameOver (False, 1)
        else if result == 2
            then gameOver (False, 2)
        else do
            putStrLn "This room appears to be empty."
            gameAction p m w

gameAction :: Player -> Map -> Wumpus -> IO ()
gameAction p m w = do
    putStrLn "Would you like to move or shoot and in which direction? (move/shoot) (right/left/back)"
    line <- getLine
    let actions = words line
    if head actions == "move" 
        then do
            thePlayer <- movePlayer (last actions) p m
            if location thePlayer == wloc w then
                gameOver (False, 0)
            else
                gameLoop thePlayer m (Wumpus (wloc w) 0)
        else if head actions == "shoot"
            then do
                (thePlayer, b) <- shootArrow (last actions) p m w
                if b 
                    then
                        gameOver (True, 0)
                    else if arrows thePlayer == 0 then
                        gameOver (False, 2)
                    else do
                        movW <- randomRIO (0, 2)
                        gameLoop thePlayer m (Wumpus (connections (m !! (wloc w - 1)) !! movW) 1)
        else do
            putStrLn "Sorry, that is not an option."
            gameLoop p m (Wumpus (wloc w) 0)

gameOver :: (Bool, Int) -> IO ()
gameOver (b, i) = do
    putStrLn (
        if b 
            then "There is a moaning sound in the next room before you hear something large crash to the ground. You killed the Wumpus! You win!" 
            else if i == 1 
                then "You walked into room with the wumpus! He ate you. Oops."
            else if i == 2 
                then "The Wumpus must have been scared by the sound of your arrow because he wondered into your room and ate you! Oops."
            else
                "You ran out of arrows! Game over!")
    startOver

startOver :: IO ()
startOver = do
    putStrLn "Would you like to play again? (yes/no)"
    line <- getLine
    if line == "yes"
        then main
        else if line == "no" 
            then putStrLn "Thanks for playing!"
        else do
            putStrLn "Sorry, that's not an aption."
            startOver
