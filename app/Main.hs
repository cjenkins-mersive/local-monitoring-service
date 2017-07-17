module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import System.Environment as E
import System.FSNotify as FSN
import System.Directory (doesDirectoryExist)

import Lib

-- fsnotify
-- websocket client lib

{- Watch directory
If new file start 'tailing'
Within 'tailing' when have 2 lines
Verify with checksum 2 lines
-}

{- Start websocket
   Pass verified lines to websocket
-}
  
startWatching :: FilePath -> IO ()
startWatching dirPath =
  FSN.withManager $ \mgr -> do
    FSN.watchDir mgr dirPath (const True) print

    --sleep until interrupted
    forever $ threadDelay 1000000

main :: IO ()
main =
  E.getArgs >>= \args ->
                  (doesDirectoryExist $ head args) >>= \dirExists ->
                                                         case dirExists of
                                                           True -> startWatching $ head args
                                                           False -> putStrLn "Directory does not exist."
