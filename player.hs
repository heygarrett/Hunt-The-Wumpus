module Player
    (
    ) where

import Map

data Player = Player { location :: Int, arrows :: Int, sense :: Int } 

move :: String -> Int
move direction
    | direction == "right" = 
    | direction == "left" = 
    | direction == "back" = 
