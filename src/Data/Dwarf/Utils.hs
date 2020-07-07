module Data.Dwarf.Utils where

import           Data.Binary.Get (getByteString, getWord8, Get, runGet)
import qualified Data.Binary.Get as Get
import           Data.Bits ((.|.), shiftL, clearBit, testBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64)
import           Data.Word (Word64)

whileJust :: (Applicative m, Monad m) => m (Maybe a) -> m [a]
whileJust act =
  let go p = do
        res <- act
        case res of
          Nothing -> pure (reverse p)
          Just x -> go (x:p)
   in go []

-- Repeatedly perform the get operation until the boolean fails.
whileM :: (Applicative m, Monad m) => (a -> Bool) -> m a -> m [a]
whileM cond act =
  let go p = do
        x <- act
        case cond x of
          False -> pure (reverse p)
          True ->  go (x:p)
   in go []

getWhileNotEmpty :: Get a -> Get [a]
getWhileNotEmpty act =
  let go p = do
        e <- Get.isEmpty
        case e of
          True -> pure (reverse p)
          False -> act >>= \x -> go (x:p)
   in go []

getByteStringLen :: Integral a => Get a -> Get B.ByteString
getByteStringLen lenGetter = getByteString =<< fromIntegral <$> lenGetter

getByteStringNul :: Get B.ByteString
getByteStringNul = L.toStrict <$> Get.getLazyByteStringNul

-- | Decode a signed little-endian base 128 encoded integer.
getSLEB128 :: Get Int64
getSLEB128 =
    let go acc shift = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift)
        if testBit byte 7 then
            go temp (shift + 7)
         else
            if shift < 32  && testBit byte 6 then
                pure $ fromIntegral $ temp .|. (maxBound `shiftL` (shift + 6))
             else
                pure $ fromIntegral temp
    in go 0 0

-- Decode an unsigned little-endian base 128 encoded integer.
getULEB128 :: Get Word64
getULEB128 =
    let go acc shift = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift)
        if testBit byte 7 then
            go temp (shift + 7)
         else
            pure temp
    in go 0 0

getAt :: Get a -> Word64 -> B.ByteString -> a
getAt action offset = strictGet $ Get.skip (fromIntegral offset) *> action

strictGet :: Get a -> B.ByteString -> a
strictGet action bs = runGet action $ L.fromChunks [bs]
