
module PngData where

import qualified Data.ByteString as B
import qualified Numeric as N


type Hex = String


data PngFile = PngFile {
    fSignature :: Signature,
    fHeader    :: Header,
    fChunks    :: [Chunk]
}


data Signature = Signature {
    hCode :: Hex,
    hExt  :: String,
    hCLRF :: Hex
} deriving (Show)


data Header = Header {
    dLength    :: Int,
    dType      :: String,
    dWidth     :: Int,
    dHeight    :: Int,
    dDepth     :: Int,
    dColor     :: Int,
    dCompress  :: Int,
    dFilter    :: Int,
    dInterlace :: Int,
    dCRC       :: B.ByteString
} deriving (Show)


data Chunk = Chunk {
    cLength :: Int,
    cType   :: String,
    cData   :: B.ByteString,
    cCRC    :: B.ByteString
} deriving (Show)




instance Show PngFile where
  show x = show(dWidth $ fHeader x) ++
           " x " ++
           show(dHeight $ fHeader x) ++
           " PNG File, " ++
           (show (1 + length (fChunks x))) ++
           " chunks"


showTypes :: PngFile -> [String]
showTypes = (map cType).fChunks

showData :: PngFile -> B.ByteString
showData = B.concat.(map cData).(filter isDAT).fChunks
  where isDAT x = cType x == "IDAT"

showHex :: B.ByteString -> [String]
showHex = map (toHex.toInteger) <$> B.unpack
  where toHex x = N.showHex x ""



--instance Show Chunk where
--  show x = chkType x
--