
module PngData where

import qualified Data.ByteString as B


type Hex = String


data PngFile = PngFile {
    fHeader      :: FileHeader,
    fHeaderChunk :: HeaderChunk,
    fChunks      :: [Chunk]
}


data FileHeader = FileHeader {
    hCode :: Hex,
    hExt  :: String,
    hCLRF :: Hex
} deriving (Show)


data HeaderChunk = HeaderChunk {
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
  show x = show(dWidth $ fHeaderChunk x) ++
           " x " ++
           show(dHeight $ fHeaderChunk x) ++
           " PNG File, " ++
           (show (1 + length (fChunks x))) ++
           " chunks"

--instance Show Chunk where
--  show x = chkType x
--