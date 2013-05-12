module Paths_wumpus (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/Garrett/.cabal/bin"
libdir     = "/Users/Garrett/.cabal/lib/wumpus-0.1.0.0/ghc-7.4.2"
datadir    = "/Users/Garrett/.cabal/share/wumpus-0.1.0.0"
libexecdir = "/Users/Garrett/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wumpus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wumpus_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wumpus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wumpus_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
