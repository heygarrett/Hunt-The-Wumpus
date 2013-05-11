module Map 
    ( Map 
    , Room ( .. )
    , numberRooms
    , generateMap
    , Wumpus ( .. )
    ) where

-- | Data types
data Room = Room { number :: Int, connections :: [Int] }
instance Show Room where
    show r = "Room number: " ++ show (number r) ++ ", " ++ "Connected rooms: " ++ show (connections r) ++ "\n"
type Map = [Room]
    
data Wumpus = Wumpus { wloc :: Int, moved :: Int }

-- | Map functions

numberRooms :: IO Int
numberRooms = do
    putStrLn "What size would you like your map? "
    putStrLn "(small/medium/large)"
    line <- getLine
    if line == "small" 
        then return 6
        else if line == "medium" 
            then return 12
        else if line == "large" 
            then return 20
        else do
            putStrLn "Sorry, but that's not a map size! "
            numberRooms

generateMap :: Int -> Int -> Map
generateMap x half
    | x == (2 * half) = generateMap (x - 1) half ++ [Room x [1, x - half, x - 1]]
    | x > half = generateMap (x - 1) half ++ [Room x [x - half, x - 1, x + 1]]
    | x == 1 = [Room 1 [2, 1 + half, 2 * half]]
    | otherwise = generateMap (x - 1) half ++ [Room x [x - 1, x + 1, x + half]]
