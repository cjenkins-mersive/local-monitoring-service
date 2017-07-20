module Lib
    ( validateChecksum,
      convertToLines,
      ensureEvenNumberOfLines
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.CRC32 as CRC

validateChecksum :: B.ByteString -> B.ByteString -> Bool
validateChecksum checksum lineOfData =
  toInteger readChecksum == toInteger (CRC.crc32 lineOfData)
  where Just(readChecksum, _) = B.readInt checksum

convertToLines :: B.ByteString -> B.ByteString -> (B.ByteString, [B.ByteString])
convertToLines state input = do
  let entireInput = B.concat [state, input]
  let lines = B.split '\n' entireInput
  if B.last input == '\n'
  then (B.empty, lines)
  else (last lines, init lines)

ensureEvenNumberOfLines :: (B.ByteString, [B.ByteString]) -> (B.ByteString, [B.ByteString])
ensureEvenNumberOfLines state = do
  let stateString = fst state
  let stateList = snd state
  if mod (length stateList) 2 == 0
  then state
  else (B.concat [last stateList, stateString], init stateList)
    
