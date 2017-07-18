module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import System.Environment as E
import System.FSNotify as FSN
import System.Directory (doesDirectoryExist)

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.CRC32 as CRC

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

processDataPair :: B.ByteString -> B.ByteString -> IO ()
processDataPair checksum lineOfData =
  if validateChecksum checksum lineOfData
  then print lineOfData
  else print "Checksum failed."

processDataFile :: FilePath -> IO ()
processDataFile dataFilePath = do
  output <- B.lines <$> B.readFile dataFilePath
  print output

watchAction :: FSN.Action
watchAction event =
  processDataFile $ FSN.eventPath event

watchPredicate :: FSN.ActionPredicate
watchPredicate(Added _ _) = True
watchPredicate(Modified _ _) = True
watchPredicate(Removed _ _) = False
  
startWatching :: FilePath -> IO ()
startWatching dirPath =
  FSN.withManager $ \mgr -> do
    FSN.watchDir mgr dirPath watchPredicate watchAction

    --sleep until interrupted
    forever $ threadDelay 1000000

main :: IO ()
main =
  E.getArgs >>= \args ->
                  (doesDirectoryExist $ head args) >>= \dirExists ->
                                                         case dirExists of
                                                           True -> startWatching $ head args
                                                           False -> putStrLn "Directory does not exist."
