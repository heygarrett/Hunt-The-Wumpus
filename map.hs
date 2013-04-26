module Map 
    ( Map 
    , Room ( .. )
    , generateMap
    , listAdj
    ) where

data Room = Room { number :: Int, connections :: [Int] }
instance Show Room where
    show r = "Room number: " ++ show (number r) ++ ", " ++ "Connected rooms: " ++ show (connections r) ++ "\n"
type Map = [Room]
    
generateMap :: Int -> Int -> Map
generateMap x half
    | x == (2 * half) = generateMap (x - 1) half ++ [Room x [1, x - half, x - 1]]
    | x > half = generateMap (x - 1) half ++ [Room x [x - half, x - 1, x + 1]]
    | x == 1 = [Room 1 [2, 1 + half, 2 * half]]
    | otherwise = generateMap (x - 1) half ++ [Room x [x - 1, x + 1, x + half]]

listAdj :: Int -> Map -> [Int]
listAdj x y = connections $ y !! (x - 1)

getSense :: Int -> Map -> String
