module Lib
    ( validateChecksum
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.CRC32 as CRC

validateChecksum :: B.ByteString -> B.ByteString -> Bool
validateChecksum checksum lineOfData =
  toInteger readChecksum == toInteger (CRC.crc32 lineOfData)
  where Just(readChecksum, _) = B.readInt checksum
