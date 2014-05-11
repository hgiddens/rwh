import qualified Data.ByteString.Lazy as L
import GHC.Word (Word8)

hasMagicFour :: [Word8] -> L.ByteString -> Bool
hasMagicFour is b = (==) (L.pack is) (L.take 4 b)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic = hasMagicFour [0x7f, 0x45, 0x4c, 0x46]

filep :: (L.ByteString -> Bool) -> FilePath -> IO Bool
filep p path = fmap p (L.readFile path)

isElfFile :: FilePath -> IO Bool
isElfFile = filep hasElfMagic

hasMachOMagic :: L.ByteString -> Bool
hasMachOMagic = hasMagicFour [0xcf, 0xfa, 0xed, 0xfe]

isMachOFile :: FilePath -> IO Bool
isMachOFile = filep hasMachOMagic
