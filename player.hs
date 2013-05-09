module Player
    ( Player ( .. )
    , getAdjRooms
    , movePlayer
    , shootArrow
    , checkForWumpus
    ) where

import System.IO
import Map

data Player = Player { location :: Int, prevLoc :: Int, arrows :: Int } 
instance Show Player where
    show p = "Player location: " ++ show (location p) ++ ", " ++ "Previous location: " ++ show (prevLoc p) ++ ", " ++ "Arrows: " ++ show (arrows p)

-- | Player functions
getAdjRooms :: Player -> Map -> [Int]
getAdjRooms p m = connections $ m !! (location p - 1)

movePlayer :: Player -> Map -> IO Player
movePlayer p m = do
    putStrLn "Would you like to go left, right, or back?"
    line <- getLine
    if line == "back" then
        return $ Player (prevLoc p) (location p) (arrows p)
    else if line == "left" then
        if prevLoc p == getAdjRooms p m !! 1 then
            return $ Player (maximum $ getAdjRooms p m) (location p) (arrows p)
        else if prevLoc p == getAdjRooms p m !! 2 then
            return $ Player (minimum $ getAdjRooms p m) (location p) (arrows p)
        else
            return $ Player (getAdjRooms p m !! 1) (location p) (arrows p)
    else if line == "right" then
        if prevLoc p == getAdjRooms p m !! 1 then
            return $ Player (minimum $ getAdjRooms p m) (location p) (arrows p)
        else if prevLoc p == getAdjRooms p m !! 2 then
            return $ Player (getAdjRooms p m !! 1) (location p) (arrows p)
        else
            return $ Player (maximum $ getAdjRooms p m) (location p) (arrows p)
    else do
        putStrLn "Sorry, that isn't a direction."
        movePlayer p m

shootArrow ::  Player -> Wumpus -> IO Player
shootArrow p w = do
    putStrLn "Would you like to shoot an arrow left, right or back?"
    line <- getLine
    putStrLn ("You have " ++ show (arrows p - 1) ++ " left in your quiver.")
    return $ Player (location p) (prevLoc p) (arrows p - 1)
    
checkForWumpus :: Player -> Map -> Wumpus -> IO ()
checkForWumpus p m w =
    if wloc w `elem` getAdjRooms p m then
        putStrLn "There is a foul stench in the air..."
    else do
        putStr "This room seems to be empty. "
        hFlush stdout
