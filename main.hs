module Main 
    ( main
    ) where

import Control.Monad
import System.IO
import Map
import Player

main :: IO ()
main = do
    putStrLn "Please specify how many rooms you'd like the map to have (must be even integer)."
    line <- fmap read getLine
    let theMap = generateMap line (div line 2)
    let thePlayer = createPlayer 1 20 3
    let theCurrent = currentLocation thePlayer
    print theMap
    forever $ do
        putStr "You are in room number " 
        hFlush stdout
        print $ show theCurrent
        putStrLn "Would you like to go right, left, or back?"
        direction <- getLine
        let newPlayer = movePlayer direction thePlayer theMap
        let thePlayer = newPlayer
        let newCurrent = currentLocation thePlayer
        let theCurrent = newCurrent
        print $ show theCurrent
        putStrLn ""
