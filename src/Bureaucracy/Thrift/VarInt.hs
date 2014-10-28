{-# LANGUAGE BangPatterns #-}


-- | Module by Jacob Stanley (jystic) that parses 
-- Thrift Compact Protocol varints efficiently.
module Bureaucracy.Thrift.VarInt 
( getVarInt32
, getVarInt64
, Bureaucracy.Thrift.VarInt.parse
, VarInt()
) where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Binary
import Data.Bits
import Data.Int
import Data.Word ()


class VarInt a where
  parse :: Parser a

instance VarInt Word16 where
  parse = getVarInt16

instance VarInt Int32 where
  parse = fromIntegral <$> getVarInt32

instance VarInt Word32 where
  parse = getVarInt32

instance VarInt Word64 where
  parse = getVarInt64

getVarInt16 :: Parser Word16
getVarInt16 = go step (stepLast "getVarInt16: varint was larger than 16-bits") 0
  where
    go :: (Int -> b -> b) -> (Int -> b) -> b
    go !s !sl = s 0 . s 7 $ sl 14
    {-# INLINE go #-}
{-# INLINE getVarInt16 #-}

getVarInt32 :: Parser Word32
getVarInt32 = go step (stepLast "getVarInt32: varint was larger than 32-bits") 0
  where
    go :: (Int -> b -> b) -> (Int -> b) -> b
    go !s !sl = s 0 . s 7 . s 14 . s 21 $ sl 28
    {-# INLINE go #-}
{-# INLINE getVarInt32 #-}
 
getVarInt64 :: Parser Word64
getVarInt64 = go step (stepLast "getVarInt64: varint was larger than 64-bits") 0
  where
    go :: (Int -> b -> b) -> (Int -> b) -> b
    go !s !sl = s  0 . s  7 . s 14 . s 21 . s  28
              . s 35 . s 42 . s 49 . s 56 $ sl 63
    {-# INLINE go #-}
{-# INLINE getVarInt64 #-}
 
step :: (Bits a, Num a) => Int -> (a -> Parser a) -> a -> Parser a
step !shbits !next !val = do
    x <- anyWord8
    if testBit x 7
       then next (val .|. (fromIntegral (x .&. 0x7F) `shiftL` shbits))
       else return $! val .|. (fromIntegral x `shiftL` shbits)
{-# INLINE step #-}
 
stepLast :: (Bits b, Num b) => String -> Int -> b -> Parser b
stepLast !failMsg !shbits !val = do
    b <- anyWord8
    if testBit b 7
       then fail failMsg
       else return $! val .|. (fromIntegral b `shiftL` shbits)
{-# INLINE stepLast #-}