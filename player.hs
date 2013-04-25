module Player
    ( Player
    , createPlayer
    , currentLocation
    , movePlayer
    ) where

import Map

data Player = Player { location :: Int, prevLoc :: Int, arrows :: Int } 
instance Show Player where
    show p = "Player location: " ++ show (location p) ++ ", " ++ "Previous location: " ++ show (prevLoc p) ++ ", " ++ "Arrows: " ++ show (arrows p) ++ "\n"

createPlayer :: Int -> Int -> Int -> Player
createPlayer x y z = Player x y z

currentLocation :: Player -> Int
currentLocation player = location player

-- |This function is not working correctly.
movePlayer :: String -> Player -> Map -> Player
movePlayer dir player map
    | dir == "back" = Player (prevLoc player) (location player) (arrows player)
    | otherwise = Player 50 50 50
