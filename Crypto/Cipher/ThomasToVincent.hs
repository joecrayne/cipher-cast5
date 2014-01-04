{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Cipher.ThomasToVincent where

import Crypto.Cipher.Types
import Data.Byteable

import Data.Tagged
import qualified Crypto.Classes as Thomas

newtype ThomasToVincent b = ThomasToVincent b

instance Thomas.BlockCipher b => Cipher (ThomasToVincent b) where
    cipherName _ = "ThomasToVincent"
    cipherInit k = ThomasToVincent b
      where Just b = Thomas.buildKey (toBytes k)
    cipherKeySize _ =  KeySizeFixed (bitlen `div` 8)
      where Tagged bitlen = Thomas.keyLength :: Tagged b Int

instance Thomas.BlockCipher b => BlockCipher (ThomasToVincent b) where
    blockSize _ = bitlen `div` 8
     where Tagged bitlen = Thomas.blockSize :: Tagged b Int
    ecbEncrypt (ThomasToVincent k) = Thomas.ecb k
    ecbDecrypt (ThomasToVincent k) = Thomas.unEcb k

