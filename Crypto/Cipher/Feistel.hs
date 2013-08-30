{-# LANGUAGE UnboxedTuples #-}
module Crypto.Cipher.Feistel where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Bits
import Data.Binary
import System.Endian

toW32Pair :: B.ByteString -> ( Word32, Word32 )
toW32Pair b = let (x1, x2) = B.splitAt 4 b
                  w1 = decode32be x1
                  w2 = decode32be x2
              in ( w1,w2 )

fromW32Pair :: (Word32, Word32) -> B.ByteString
fromW32Pair (w1,w2)
    = let w1' = fromIntegral w1
          w2' = fromIntegral w2
          w = (w1' `shiftL` 32) .|. w2'
      in encode64be w


decode32be :: B.ByteString -> Word32
decode32be s = id $!
    (fromIntegral (s `B.index` 0) `shiftL` 24) .|.
    (fromIntegral (s `B.index` 1) `shiftL` 16) .|.
    (fromIntegral (s `B.index` 2) `shiftL`  8) .|.
    (fromIntegral (s `B.index` 3) )

encode64be :: Word64 -> B.ByteString
encode64be w = B.pack . map fromIntegral $
                [ (w `shiftR` 56) .&. 0xff
                , (w `shiftR` 48) .&. 0xff
                , (w `shiftR` 40) .&. 0xff
                , (w `shiftR` 32) .&. 0xff
                , (w `shiftR` 24) .&. 0xff
                , (w `shiftR` 16) .&. 0xff
                , (w `shiftR` 8) .&. 0xff
                , w .&. 0xff
                ]

splitWord32 :: Word32 -> (# Word8, Word8, Word8, Word8 #)
splitWord32 w = (# a,b,c,d #)
 where
    a = fromIntegral $ w `shiftR` 24
    b = fromIntegral $ w `shiftR` 16
    c = fromIntegral $ w `shiftR` 8
    d = fromIntegral $ w
