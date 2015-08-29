
module Png where

import Numeric
import Data.List
import qualified Data.ByteString as B

import BStringParse
import PngData


test :: State String
test = do
  x <- getString 2
  y <- getInt 2
  return $ x ++ " " ++ show y


parseSignature :: State Signature
parseSignature = do
  x1 <- getHex 1
  x2 <- getString 3
  x3 <- getHex 4
  return $ Signature x1 x2 x3


parseHeader :: State Header
parseHeader = do
  len <- getInt 4
  typ <- getString 4
  x1 <- getInt 4
  x2 <- getInt 4
  x3 <- getInt 1
  x4 <- getInt 1
  x5 <- getInt 1
  x6 <- getInt 1
  x7 <- getInt 1
  crc <- getBString 4
  return $ Header len typ x1 x2 x3 x4 x5 x6 x7 crc


parseChunk :: State Chunk
parseChunk = do
  len <- getInt 4
  typ <- getString 4
  dat <- getBString len
  crc <- getBString 4
  return $ Chunk len typ dat crc


parseChunks :: State [Chunk]
parseChunks = do loop []
  where
    loop chunks = do
      chunk <- parseChunk
      let chunks' = chunks ++ [chunk]
      if (cType chunk) == "IEND"
      then return chunks'
      else loop $ chunks'
      

readPng :: String -> IO PngFile
readPng fname = do
        stream <- B.readFile fname
        return $ run parser stream
        where
          parser = do
            hdr <- parseSignature
            dhdr <- parseHeader
            chunks <- parseChunks
            return $ PngFile hdr dhdr chunks

