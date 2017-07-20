module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.QSem
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import System.Environment as E
import System.FSNotify as FSN
import System.Directory (doesDirectoryExist)
import System.IO (withFile, IOMode(ReadMode), stdout)
import System.FilePath

import Streaming
import qualified Streaming.Prelude as S

import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Char8 (lines)
import qualified Data.ByteString.Lazy.Char8 as LBS
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

tailing :: FilePath -> (BS.ByteString IO () -> IO r) -> IO r
tailing filepath continuation = FSN.withManager $ \mgr -> do
  let dir = fst $ splitFileName filepath
  let watchDirPredicate = tailPredicate filepath
  sem <- newQSem 1
  FSN.watchDir mgr dir watchDirPredicate (\_ -> signalQSem sem)
  withFile filepath ReadMode (\h -> continuation (handleToStream sem h))
  where
  handleToStream sem h = BS.concat . Streaming.repeats $ do
    lift (waitQSem sem)
    readWithoutClosing h
  -- Can't use B.fromHandle here because annoyingly it closes handle on EOF
  -- instead of just returning, and this causes problems on new appends.
  readWithoutClosing h = do
    c <- lift (B.hGetSome h defaultChunkSize)
    if B.null c
      then return ()
      else do BS.chunk c
              readWithoutClosing h

tailingContinuation :: WS.Connection -> BS.ByteString IO r -> IO r
tailingContinuation conn = BS.stdout

processDataPair :: LBS.ByteString -> LBS.ByteString -> IO ()
processDataPair checksum lineOfData =
  if validateChecksum checksum lineOfData
  then print lineOfData
  else print "Checksum failed."

watchAction :: WS.Connection -> FSN.Action
watchAction conn (Added eventPath _) = tailing eventPath (tailingContinuation conn)
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

  _ <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg
  
  startWatching watchDir (watchAction conn)
  
  --let loop = do
  --      line <- "Get data from stream here"
  --      unless (LBS.null line) $ WS.sendTextData conn line >> loop
  --loop
  
  WS.sendClose conn("EOS" :: Text)

main :: IO ()
main =  
  E.getArgs >>= \args ->
                  (doesDirectoryExist $ head args) >>= \dirExists ->
                                                         case dirExists of
                                                           True -> withSocketsDo $ WS.runClient "127.0.0.1" 80 "/" (app $ head args)
                                                           False -> putStrLn "Directory does not exist."
