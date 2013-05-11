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
    show p = "Player location: " ++ show (location p) ++ ", " ++ "previous location: " ++ show (prevLoc p) ++ ", " ++ "arrows: " ++ show (arrows p)

-- | Player functions
getAdjRooms :: Player -> Map -> [Int]
getAdjRooms p m = connections $ m !! (location p - 1)

movePlayer :: String -> Player -> Map -> IO Player
movePlayer s p m = do
    r <- getDirection s p m
    if r == 0 then do
        putStrLn "Sorry, that is not a direction."
        putStrLn "Which direction would you like to move?"
        line <- getLine
        movePlayer line p m 
    else
        return $ Player r (location p) (arrows p)

shootArrow :: String -> Player -> Map -> Wumpus -> IO (Player, Bool)
shootArrow s p m w = do
    r <- getDirection s p m
    if r == 0 then do
        putStrLn "Sorry, that's not a direction."
        putStrLn "Which direction would you like to shoot?"
        line <- getLine
        shootArrow line p m w
    else if wloc w == r then
        return (p, True)
    else do
        putStr ("You hear the clatter of your arrow in the next room. You have " ++ show (arrows p - 1) ++ " left in your quiver. ")
        hFlush stdout
        return (Player (location p) (prevLoc p) (arrows p - 1), False)

getDirection ::  String -> Player -> Map -> IO Int
getDirection s p m = 
    if s == "" then
        return 7
    else if s == "back" then
        return $ prevLoc p
    else if s == "left" then
        if prevLoc p == getAdjRooms p m !! 1 then
            return $ maximum (getAdjRooms p m)
        else if prevLoc p == head (getAdjRooms p m) then
            return $ getAdjRooms p m !! 1
        else
            return $ minimum (getAdjRooms p m)
    else if s == "right" then
        if prevLoc p == getAdjRooms p m !! 1 then
            return $ minimum (getAdjRooms p m)
        else if prevLoc p == head (getAdjRooms p m) then
            return $ maximum (getAdjRooms p m)
        else
            return $ getAdjRooms p m !! 1
    else
        return 0

checkForWumpus :: Player -> Map -> Wumpus -> IO Int
checkForWumpus p m w =
    if wloc w `elem` getAdjRooms p m then do
        putStrLn "There is a foul stench in the air..."
        return 2
    else if wloc w == location p then
        if moved w == 0 then
            return 0
        else
            return 1
    else do
        putStr "This room seems to be empty. "
        hFlush stdout
        return 2
