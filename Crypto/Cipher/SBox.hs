{-# LANGUAGE TemplateHaskell #-}
module Crypto.Cipher.SBox (SBox,(#),sbox,pbox,bytes,parseBox,parseBytes) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Bits
import Data.Binary
import Data.Char
import Data.Word
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as S

unsupported _ = fail "not supported."

str  = QuasiQuoter { 
        quoteExp = stringE, 
        quotePat = unsupported, 
        quoteDec = unsupported, 
        quoteType = unsupported 
       }
sbox = str { quoteExp = sboxExp }
pbox = sbox
bytes = str { quoteExp = bytesExp }

sboxExp st  = let w=mkName "parseBox"   in return $ AppE (VarE w) (LitE (StringL st))
bytesExp st = let w=mkName "parseBytes" in return $ AppE (VarE w) (LitE (StringL st))

type SBox = V.Vector Word32

{-# INLINE (#) #-}
(#) :: SBox -> Word8 -> Word32
v # i = v V.! (fromIntegral i )


parseBox :: String -> SBox
parseBox xs = V.fromList $ (map (read . ("0x"++) {- . reverseNibbles -} ) . words $ xs :: [Word32])
 where
    reverseNibbles = concat .  reverse .  group2 (\a b->[a,b])

    group2 f (x:y:ys) = f x y : group2 f ys
    group2 _ _        = []

parseBytes bs = S.pack . parseHex' . concat . words $ bs 
 where
  parseHex' bs =
      let (dnib,ts) = splitAt 2 bs
          parseNibble x = group2 toW8 $ map (hexDigit . ord8) x
          hexDigit d = d - (if d>0x39 then if d<0x61 then 0x37 else 0x57 else 0x30)
          group2 f (x:y:ys) = f x y : group2 f ys
          group2 _ _        = []
          toW8 a b = shift a 4 .|. b
          ord8 c = fromIntegral . ord $ c :: Word8
      in parseNibble dnib ++
          if null ts
              then []
              else parseHex' ts


