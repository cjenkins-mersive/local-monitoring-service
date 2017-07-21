module Lib
    ( validateChecksum,
      convertToLines,
      ensureEvenNumberOfLines,
      validateLines
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Digest.CRC32 as CRC
import Data.List.Split (chunksOf)

validateChecksum :: B.ByteString -> B.ByteString -> Bool
validateChecksum checksum lineOfData =
  toInteger readChecksum == toInteger (CRC.crc32 lineOfData)
  where Just(readChecksum, _) = C.readInt checksum

convertToLines :: B.ByteString -> B.ByteString -> (B.ByteString, [B.ByteString])
convertToLines state input = do
  let cleanInput = C.filter (\c -> c /= '\r') input
  let entireInput = B.concat [state, cleanInput]
  let lines = filter (\b -> B.length b > 0) $ C.split '\n' entireInput
  if C.last input == '\n'
  then (B.empty, lines)
  else (last lines, init lines)

ensureEvenNumberOfLines :: (B.ByteString, [B.ByteString]) -> (B.ByteString, [B.ByteString])
ensureEvenNumberOfLines state = do
  let stateString = fst state
  let stateList = snd state
  if mod (length stateList) 2 == 0
  then state
  else (B.concat [last stateList, C.pack("\n"), stateString], init stateList)
    
validateLines :: [B.ByteString] -> [B.ByteString]
validateLines lines = do
  let dataPairs = chunksOf 2 $ lines
  let filteredPairs = filter (\pair -> validateChecksum (head pair) (last pair)) dataPairs
  map (\pair -> last pair) filteredPairs
