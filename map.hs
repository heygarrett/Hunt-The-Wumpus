module Map 
    ( Map 
    , Room
    , generateMap
    ) where

data Room = Room { number :: Int, connections :: [Int] }
instance Show Room where
    show r = "Room number: " ++ show (number r) ++ ", " ++ "Connected rooms: " ++ show (connections r) ++ "\n"
type Map = [Room]

-- Argument provided must be 1
generateMap :: Int -> Map
generateMap x 
    | x == 1 = Room 1 [2, 11, 20] : generateMap 2
    | x == 20 = [Room 20 []]
    | otherwise = Room x ((x + 1) : c) : generateMap (x + 1) where
        c = if x > 10 then [] else [x + 10]
