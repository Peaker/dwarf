module Data.Dwarf.Utils where

import Control.Applicative (Applicative(..), (<$>), (*>))
import Data.Binary.Get (getByteString, getWord8, Get, runGet)
import Data.Bits ((.|.), shiftL, clearBit, testBit)
import Data.Int (Int64)
import Data.Word (Word64)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8

whileJust :: (Applicative m, Monad m) => m (Maybe a) -> m [a]
whileJust act = go
  where
    go = do
      res <- act
      case res of
        Nothing -> pure []
        Just x -> (x :) <$> go

-- Repeatedly perform the get operation until the boolean fails.
whileM :: (Applicative m, Monad m) => (a -> Bool) -> m a -> m [a]
whileM cond act = whileJust $ do
  res <- act
  pure $
    if cond res
    then Just res
    else Nothing

condAct :: (Applicative m, Monad m) => m Bool -> m a -> m (Maybe a)
condAct cond act = do
  e <- cond
  if e
    then pure Nothing
    else Just <$> act

getWhileNotEmpty :: Get a -> Get [a]
getWhileNotEmpty = whileJust . condAct Get.isEmpty

getByteStringLen :: Integral a => Get a -> Get B.ByteString
getByteStringLen lenGetter =
  getByteString =<< fromIntegral <$> lenGetter

getUTF8Str0 :: Get String
getUTF8Str0 = UTF8.toString . B.pack <$> whileM (/= 0) getWord8

getNonEmptyUTF8Str0 :: Get (Maybe String)
getNonEmptyUTF8Str0 = do
  str <- getUTF8Str0
  pure $ if null str then Nothing else Just str

-- Decode a signed little-endian base 128 encoded integer.
getSLEB128 :: Get Int64
getSLEB128 =
    let go acc shift = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift)
        if testBit byte 7 then
            go temp (shift + 7)
         else
            if shift < 32  && testBit byte 6 then
                pure $ fromIntegral $ temp .|. ((-1) `shiftL` (shift + 6))
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
