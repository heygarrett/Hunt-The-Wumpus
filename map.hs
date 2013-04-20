module Map 
    ( Map 
    , Room
    , createRooms
    ) where

import System.Random

data Room = Room { number :: Int, connections :: [Int] }
type Map = [Room]

createRooms :: Int -> Map
createRooms 0 = []
createRooms x = let rooms = Room x [] : createRooms (x - 1)
