{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import System.Environment as E
import System.Directory (doesDirectoryExist)
import System.IO (withFile, IOMode(ReadMode), stdout, Handle)
import System.FilePath

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 (lines)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.ByteString.Lazy (toStrict)
import Data.Digest.CRC32 as CRC
import Data.Text (Text)
import qualified Data.Text.IO as T

import Network.Connection as C
import Network.Socket (withSocketsDo)
import Network.WebSockets as WS
import Network.WebSockets.Stream as WS.Stream
import Wuss as WSS

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

tailing :: FilePath -> WS.Connection -> IO r
tailing filepath conn = do
  print ("Started tailing file" ++ show filepath)
  let dir = fst $ splitFileName filepath
  let initialState = B.empty
  let initialResults = []
  withFile filepath ReadMode (\h -> handleToStream initialState initialResults h conn)

handleToStream :: B.ByteString -> [B.ByteString] -> Handle -> WS.Connection -> IO r
handleToStream state results h conn = do
  let currentData = readWithoutClosing state results h
  currentData >>= \(newState, newResults) -> do
    sendToServer conn newResults
    handleToStream newState [] h conn
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

app :: FilePath -> WS.ClientApp ()
app watchDir conn = do
  print "Websocket connected!"
  
  --startWatching watchDir (watchAction conn)
  tailing watchDir conn
  
  WS.sendClose conn("EOS" :: Text)

startSecureWebsocket tailFilePath = do
  let host = "192.168.2.7"
  let port = 8443
  let path = "/produce"
  let options = WS.defaultConnectionOptions
  let headers = []
  let tlsSettings = C.TLSSettingsSimple {
        settingDisableCertificateValidation = True,
        settingDisableSession = False,
        settingUseServerName = False
        }
  let connectionParams = C.ConnectionParams {
        connectionHostname = host,
        connectionPort = port,
        connectionUseSecure = Just tlsSettings,
        connectionUseSocks = Nothing
        }

  context <- C.initConnectionContext
  connection <- C.connectTo context connectionParams
  stream <- WS.Stream.makeStream
    (fmap Just (C.connectionGetChunk connection))
    (maybe (return ()) (C.connectionPut connection . toStrict))
  WS.runClientWithStream stream host path options headers $ \connection -> do
    tailing tailFilePath connection
    return ()
  
main :: IO ()
main =  
  --E.getArgs >>= \args ->
  --                (doesDirectoryExist $ head args) >>= \dirExists ->
  --                                                       case dirExists of
  --                                                         True -> withSocketsDo $ WS.runClient "127.0.0.1" 8765 "/" (app $ head args)
  --                                                         False -> putStrLn "Directory does not exist."
  E.getArgs >>= \args -> startSecureWebsocket $ (head args)
