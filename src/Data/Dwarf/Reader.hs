module Data.Dwarf.Reader where

import           Data.Binary.Get
  ( Get
  , getWord16be, getWord32be, getWord64be
  , getWord16le, getWord32le, getWord64le
  )
import qualified Data.Binary.Get as Get
import           Data.Bits
import qualified Data.ByteString.Unsafe as BS
import           Data.Int (Int16, Int32, Int64)
import           Data.Word (Word16, Word32, Word64)

-- | Ordering bytes are encoded in buffers.
data Endianess = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show)

-- | Encoding of buffers
data Encoding = Encoding32 | Encoding64
  deriving (Eq, Ord, Read, Show)

-- | Width of pointers in target.
data TargetSize = TargetSize32 | TargetSize64
  deriving (Eq, Ord, Read, Show)

derGetW16 :: Endianess -> Get Word16
derGetW16 end =
  case end of
    LittleEndian -> getWord16le
    BigEndian    -> getWord16be

derGetW32 :: Endianess -> Get Word32
derGetW32 end =
  case end of
    LittleEndian -> getWord32le
    BigEndian    -> getWord32be

derGetW64 :: Endianess -> Get Word64
derGetW64 end =
  case end of
    LittleEndian -> getWord64le
    BigEndian    -> getWord64be

derGetI16 :: Endianess -> Get Int16
derGetI16 end = fromIntegral <$> derGetW16 end

derGetI32 :: Endianess -> Get Int32
derGetI32 end = fromIntegral <$> derGetW32 end

derGetI64 :: Endianess -> Get Int64
derGetI64 end = fromIntegral <$> derGetW32 end

-- | Read a 3-byte value from stream.
derGetWord3 :: Endianess -> Get Word64
derGetWord3 end = do
  bs <- Get.getByteString 3
  case end of
    LittleEndian ->
      pure $! fromIntegral (bs `BS.unsafeIndex` 2) `shiftL` 16
          .|. fromIntegral (bs `BS.unsafeIndex` 1) `shiftL`  8
          .|. fromIntegral (bs `BS.unsafeIndex` 0)
    BigEndian ->
      pure $! fromIntegral (bs `BS.unsafeIndex` 0) `shiftL` 16
          .|. fromIntegral (bs `BS.unsafeIndex` 1) `shiftL`  8
          .|. fromIntegral (bs `BS.unsafeIndex` 2)

-- Intermediate data structure for a partial Reader.
data EndianSizeReader = EndianSizeReader
  { desrEndianess  :: !Endianess
  , desrEncoding :: !Encoding
  }

endianSizeReader :: Encoding -> Endianess -> EndianSizeReader
endianSizeReader enc endianess =
  EndianSizeReader { desrEndianess = endianess
                   , desrEncoding = enc
                   }


desrGetOffset :: Endianess -> Encoding -> Get Word64
desrGetOffset endianess enc =
  case enc of
    Encoding64 -> derGetW64 endianess
    Encoding32 -> fromIntegral <$> derGetW32 endianess

instance Show EndianSizeReader where
    show desr = "EndianSizeReader " ++ show (desrEndianess desr) ++ " " ++ show (desrEncoding desr)

-- | Type containing functions and data needed for decoding DWARF information.
data Reader = Reader
    { drDesr                  :: !EndianSizeReader
    , drTarget64              :: !TargetSize
    }

instance Show Reader where
    show dr = "Reader " ++ show (drDesr dr) ++ " " ++ show (drTarget64 dr)

reader :: Endianess -> Encoding -> TargetSize -> Reader
reader endianess enc tgt =
  let esr = EndianSizeReader { desrEndianess = endianess
                             , desrEncoding = enc
                             }
   in Reader { drDesr = esr
             , drTarget64 = tgt
             }

-- | Largest permissible target address.
largestTargetAddress :: TargetSize -> Word64
largestTargetAddress tgt =
  case tgt of
    TargetSize64 -> 0xffffffffffffffff
    TargetSize32 -> 0xffffffff

-- | Action for reading a pointer for the target machine.
getTargetAddress :: Endianess -> TargetSize -> Get Word64
getTargetAddress end tgt =
  case tgt of
    TargetSize64 -> derGetW64 end
    TargetSize32 -> fromIntegral <$> derGetW32 end

drEndianess :: Reader -> Endianess
drEndianess = desrEndianess . drDesr

drEncoding :: Reader -> Encoding
drEncoding = desrEncoding . drDesr

encodingLargestOffset :: Encoding -> Word64
encodingLargestOffset Encoding64 = 2^(64::Int) - 1
encodingLargestOffset Encoding32 = 2^(32::Int) - 1

drLargestOffset :: Reader -> Word64
drLargestOffset = encodingLargestOffset . drEncoding

-- | Decode the DWARF size header entry, which specifies the encoding
-- and the size of a DWARF subsection.
getDwarfSize :: Endianess -> Get (Encoding, Word64)
getDwarfSize endianess = do
  size <- derGetW32 endianess
  if size == 0xffffffff then do
    size64 <- derGetW64 endianess
    pure (Encoding64, size64)
   else if size < 0xffffff00 then do
    pure (Encoding32, fromIntegral size)
   else
    fail $ "Invalid DWARF size: " ++ show size

-- Decode the DWARF size header entry, which specifies both the size
-- of a DWARF subsection and whether this section uses DWARF32 or
-- DWARF64.
getUnitLength :: Endianess -> Get (EndianSizeReader, Word64)
getUnitLength endianess = do
  (enc, size) <- getDwarfSize endianess
  pos <- Get.bytesRead
  pure (endianSizeReader enc endianess, fromIntegral pos + size)
