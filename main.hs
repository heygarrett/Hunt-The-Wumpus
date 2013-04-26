module Main 
    ( main
    ) where

-- |Modules for hFlush and getLine
import Control.Monad
import System.IO
-- |Import functions from Map and Player modules
import Map
import Player

main :: IO ()
main = do
    -- |Creates the map and places the user in room 1 with 20 behind and 3 arrows
    putStrLn "Please specify how many rooms you'd like the map to have (must be even integer)."
    line <- fmap read getLine
    let theMap = generateMap line (div line 2)
    let thePlayer = createPlayer 1 20 3
    print theMap
    let adj = getAdjRooms thePlayer theMap
    print adj
    forever $ do
        putStr "You are in room number " 
        hFlush stdout
        print $ location thePlayer
        print thePlayer
        putStrLn "Would you like to go right, left, or back?"
        direction <- getLine
        let newPlayer = movePlayer direction thePlayer theMap
        let thePlayer = newPlayer
        print thePlayer
        return ()
