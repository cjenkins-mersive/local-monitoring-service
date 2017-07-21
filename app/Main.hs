{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.QSem
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import System.Environment as E
import System.FSNotify as FSN
import System.Directory (doesDirectoryExist)
import System.IO (withFile, IOMode(ReadMode), stdout, Handle)
import System.FilePath

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 (lines)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Digest.CRC32 as CRC
import Data.Text (Text)
import qualified Data.Text.IO as T

import Network.Socket (withSocketsDo)
import Network.WebSockets as WS

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

tailPredicate :: FilePath -> FSN.ActionPredicate
tailPredicate filepath (Modified eventPath _) = filepath == eventPath
tailPredicate filepath (Added _ _) = False
tailPredicate filepath (Removed _ _) = False

tailing :: FilePath -> WS.Connection -> IO r
tailing filepath conn = FSN.withManager $ \mgr -> do
  print ("Started tailing file" ++ show filepath)
  let dir = fst $ splitFileName filepath
  let watchDirPredicate = tailPredicate filepath
  let initialState = B.empty
  let initialResults = []
  sem <- newQSem 1
  FSN.watchDir mgr dir watchDirPredicate (\_ -> signalQSem sem)
  withFile filepath ReadMode (\h -> handleToStream initialState initialResults sem h conn)

handleToStream :: B.ByteString -> [B.ByteString] -> QSem -> Handle -> WS.Connection -> IO r
handleToStream state results sem h conn = do
  --waitQSem sem
  let currentData = readWithoutClosing state results h
  currentData >>= \(newState, newResults) -> do
    sendToServer conn newResults
    handleToStream newState [] sem h conn
  -- Can't use B.fromHandle here because annoyingly it closes handle on EOF
  -- instead of just returning, and this causes problems on new appends.

readWithoutClosing :: B.ByteString -> [B.ByteString] -> Handle -> IO ( (B.ByteString, [B.ByteString]) )
readWithoutClosing state results h = do
  B.hGetSome h defaultChunkSize >>= \c ->
    if B.null c
    then return (state, results)
    else do
      let st = ensureEvenNumberOfLines $ convertToLines state c
      let validatedLines = validateLines (snd st)
      readWithoutClosing (fst st) (results ++ validatedLines) h

sendToServer :: WS.Connection -> [B.ByteString] -> IO ()
sendToServer conn validatedLines = do
  case (length validatedLines) > 0 of
    True -> WS.sendTextDatas conn validatedLines
    False -> threadDelay 50000

watchAction :: WS.Connection -> FSN.Action
watchAction conn (Added eventPath _) = tailing eventPath conn
watchAction conn (Modified _ _) = print "Modified event ignored."
watchAction conn (Removed _ _) = print "Removed event ignored."

watchPredicate :: FSN.ActionPredicate
watchPredicate(Added _ _) = True
watchPredicate(Modified _ _) = True
watchPredicate(Removed _ _) = False
  
startWatching :: FilePath -> FSN.Action -> IO ()
startWatching dirPath action =
  FSN.withManager $ \mgr -> do
    FSN.watchDir mgr dirPath watchPredicate action

    --sleep until interrupted
    forever $ threadDelay 1000000

app :: FilePath -> WS.ClientApp ()
app watchDir conn = do
  print "Websocket connected!"
  
  --startWatching watchDir (watchAction conn)
  tailing watchDir conn
  
  WS.sendClose conn("EOS" :: Text)

main :: IO ()
main =  
  --E.getArgs >>= \args ->
  --                (doesDirectoryExist $ head args) >>= \dirExists ->
  --                                                       case dirExists of
  --                                                         True -> withSocketsDo $ WS.runClient "127.0.0.1" 8765 "/" (app $ head args)
  --                                                         False -> putStrLn "Directory does not exist."
  E.getArgs >>= \args -> withSocketsDo $ WS.runClient "127.0.0.1" 8765 "/" (app $ head args)
