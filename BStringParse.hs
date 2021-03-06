
module BStringParse where

import Numeric
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


-- **** State Monad Joining - we want to chain the closures!! ****
-- 
-- a = value
-- b = bstring
-- c = closure (b -> (b, a))
-- S c = State a
-- f = a -> S c
--
-- 
-- s0 -> ()
-- b0 -> c0 -> (b1, a1) -> (f(a1) = S c1) -> b1 -> c1 -> (b2, a2)


type BString = B.ByteString

data State a = S (BString -> (BString,  Either String a))

instance Functor State where

  fmap fn (S c0) = S cfinal
    where
      cfinal b0 = (b1, a2)
        where
          (b1, a1) = c0 b0
          a2 = fn <$> a1
        

instance Applicative State where

  (S cfn) <*> (S c0) = S cfinal
    where
      cfinal b0 = (b2, a2)
        where
          (b1, fn) = cfn b0
          (b2, a1) = c0 b1
          a2 = fn <*> a1
          
  pure x = S (\b0 -> (b0, Right x))


instance Monad State where

  (>>=) (S c0) f = S cfinal
    where
      cfinal b0 = (b2, a2)
        where
          (b1, a1) = c0 b0
          (S c1) = checkLeft f a1
          (b2, a2) = c1 b1
          checkLeft _ (Left x) = S (\b -> (b, Left x))
          checkLeft f (Right x) = f x
          
  return a = S cfinal
    where
      cfinal b0 = (b0, Right a)


getClosure :: State a -> (BString -> (BString, Either String a))
getClosure (S x) = x


run ::  State a -> BString -> Either String a
run s b = a
  where (_, a) = getClosure s b


getBString :: Int -> State BString
getBString n = S closure
  where
    closure b = if B.length b < n
                then (b, Left "Unexpected end of stream")
                else (b', Right bytes)
      where
        (bytes, b') = B.splitAt n b

  
getInt :: Int -> State Int
getInt n = do
  bytes <- getBString n
  return $ toInt bytes 0
  where
    toInt bytes sum | B.length bytes == 0 = sum
                    | otherwise           = toInt bytes' sum'
                    where
                      (byte, bytes') = B.splitAt 1 bytes
                      sum' = (sum * 256) + (fromIntegral $ B.head byte)

  
getString :: Int -> State String
getString n = do
  bytes <- getBString n
  return $ C.unpack bytes  


getHex :: Int -> State String
getHex n = do
  int <- getInt n
  return $ showHex int ""



-------------------------------------------------------------------------

testBS = B.pack [0, 0, 0, 0]

test :: State String
test = do
  x <- getString 2
  y <- getString 4
  return $ show x ++ " " ++ show y

testParse :: BString -> Either String String
testParse = run test
