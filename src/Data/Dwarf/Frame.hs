-- | Functions for extracting data from @.eh_frame_hdr@, @.eh_frame@
-- and @.debug_frame@ sections.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Dwarf.Frame
  ( -- * .eh_frame_hdr contents
    EhFrameHdr(..)
  , ehfhFdeCount
  , getEhFrameHdr
  , EhFrameHdrBstEntry
  , ehFrameHdrBstEntry
    -- * Context
  , FrameContext(..)
    -- * Common Information Entry
  , DW_CIE(..)
  , CiePersonality(..)
  , getCIE
  , encodingSize
  , frameTotalSize
    -- * Frame Description entry
  , DW_FDE(..)
  , FDEParseError(..)
  , getFDEAt
    -- * CIE/FDE encoding
  , EhFrameHdrEnc(..)
  , pattern DW_EH_PE_absptr
  , pattern DW_EH_PE_omit
  , pattern DW_EH_PE_value
  , pattern DW_EH_PE_app
  , pattern DW_EH_PE_uleb128
  , pattern DW_EH_PE_udata2
  , pattern DW_EH_PE_udata4
  , pattern DW_EH_PE_udata8
  , pattern DW_EH_PE_sleb128
  , pattern DW_EH_PE_sdata2
  , pattern DW_EH_PE_sdata4
  , pattern DW_EH_PE_sdata8
  , pattern DW_EH_PE_pcrel
  , pattern DW_EH_PE_textrel
  , pattern DW_EH_PE_datarel
  , pattern DW_EH_PE_funcrel
  , pattern DW_EH_PE_aligned
  , pattern DW_EH_PE_indirect
  , ehFrameHdrValueSize
  ) where

import           Control.Monad
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Int
import           Data.Word
import           Numeric (showHex)

import           Data.Dwarf.Internals
import           Data.Dwarf.Reader

------------------------------------------------------------------------
-- EhFrameHdrEnc

-- | Dwarf exception header encoding information.
newtype EhFrameHdrEnc = EhFrameHdrEnc Word8
  deriving (Eq, Bits, Num)

pattern DW_EH_PE_absptr :: EhFrameHdrEnc
pattern DW_EH_PE_absptr = EhFrameHdrEnc 0

pattern DW_EH_PE_omit :: EhFrameHdrEnc
pattern DW_EH_PE_omit = EhFrameHdrEnc 0xff

pattern DW_EH_PE_value :: EhFrameHdrEnc
pattern DW_EH_PE_value = EhFrameHdrEnc 0x0f

pattern DW_EH_PE_app :: EhFrameHdrEnc
pattern DW_EH_PE_app = EhFrameHdrEnc 0xf0

-- | Unsigned value is encoded using the Little Endian Base 128
-- (LEB128) as defined by DWARF Debugging Information Format, Revision
-- 2.0.0 (July 27, 1993).
pattern DW_EH_PE_uleb128 :: EhFrameHdrEnc
pattern DW_EH_PE_uleb128 = EhFrameHdrEnc 0x01

-- | A 2 bytes unsigned value.
pattern DW_EH_PE_udata2 :: EhFrameHdrEnc
pattern DW_EH_PE_udata2 = EhFrameHdrEnc 0x02

-- | A 4 bytes unsigned value.
pattern DW_EH_PE_udata4 :: EhFrameHdrEnc
pattern DW_EH_PE_udata4 = EhFrameHdrEnc 0x03

-- | An 8 bytes unsigned value.
pattern DW_EH_PE_udata8 :: EhFrameHdrEnc
pattern DW_EH_PE_udata8 = EhFrameHdrEnc 0x04

-- | Signed value is encoded using the Little Endian Base 128 (LEB128)
-- as defined by DWARF Debugging Information Format, Revision 2.0.0
-- (July 27, 1993).
pattern DW_EH_PE_sleb128 :: EhFrameHdrEnc
pattern DW_EH_PE_sleb128 = EhFrameHdrEnc 0x09

-- | A 2 bytes signed value.
pattern DW_EH_PE_sdata2 :: EhFrameHdrEnc
pattern DW_EH_PE_sdata2 = EhFrameHdrEnc 0x0a

-- | A 4 bytes signed value.
pattern DW_EH_PE_sdata4 :: EhFrameHdrEnc
pattern DW_EH_PE_sdata4 = EhFrameHdrEnc 0x0b

-- | An 8 bytes signed value
pattern DW_EH_PE_sdata8 :: EhFrameHdrEnc
pattern DW_EH_PE_sdata8 = EhFrameHdrEnc 0x0C

pattern DW_EH_PE_pcrel :: EhFrameHdrEnc
pattern DW_EH_PE_pcrel = EhFrameHdrEnc 0x10

pattern DW_EH_PE_textrel :: EhFrameHdrEnc
pattern DW_EH_PE_textrel = EhFrameHdrEnc 0x20

pattern DW_EH_PE_datarel :: EhFrameHdrEnc
pattern DW_EH_PE_datarel = EhFrameHdrEnc 0x30

pattern DW_EH_PE_funcrel :: EhFrameHdrEnc
pattern DW_EH_PE_funcrel = EhFrameHdrEnc 0x40

pattern DW_EH_PE_aligned :: EhFrameHdrEnc
pattern DW_EH_PE_aligned = EhFrameHdrEnc 0x50

pattern DW_EH_PE_indirect :: EhFrameHdrEnc
pattern DW_EH_PE_indirect = EhFrameHdrEnc 0x80

instance Show EhFrameHdrEnc where
  showsPrec p (EhFrameHdrEnc x) = showParen (p > 5) $ showString "0x" . showHex x

-- | Return number of bytes used for ehframe header encoing.
ehFrameHdrValueSize :: TargetSize
                    -> EhFrameHdrEnc
                    -> Int
ehFrameHdrValueSize _ DW_EH_PE_omit = 0
ehFrameHdrValueSize sz enc = do
  case enc .&. DW_EH_PE_value of
    DW_EH_PE_absptr  ->
      case sz of
        TargetSize32 -> 4
        TargetSize64 -> 8
    DW_EH_PE_uleb128 -> 0
    DW_EH_PE_udata2  -> 2
    DW_EH_PE_udata4  -> 4
    DW_EH_PE_udata8  -> 8
    DW_EH_PE_sleb128 -> 0
    DW_EH_PE_sdata2  -> 2
    DW_EH_PE_sdata4  -> 4
    DW_EH_PE_sdata8  -> 8
    _ -> 0

-- | Read the header encoding without adjusting.
getEhFrameHdrUnValue :: Endianess -> TargetSize -> EhFrameHdrEnc -> Get Word64
getEhFrameHdrUnValue end tgtSize enc = do
  case enc .&. DW_EH_PE_value of
    DW_EH_PE_absptr  ->
      case tgtSize of
        TargetSize32 -> fromIntegral <$> derGetW32 end
        TargetSize64 -> derGetW64 end
    DW_EH_PE_uleb128 -> getULEB128
    DW_EH_PE_udata2  -> fromIntegral <$> derGetW16 end
    DW_EH_PE_udata4  -> fromIntegral <$> derGetW32 end
    DW_EH_PE_udata8  -> derGetW64 end
    DW_EH_PE_sleb128 -> fromIntegral <$> getSLEB128
    DW_EH_PE_sdata2  -> fromIntegral <$> derGetI16 end
    DW_EH_PE_sdata4  -> fromIntegral <$> derGetI32 end
    DW_EH_PE_sdata8  -> fromIntegral <$> derGetI64 end
    venc -> fail $ "Could not interpret encoding: " ++ show venc

-- | Get a value encoded using ehframe hdr enc, but use
getAdjustedEhFrameHdrValue :: Endianess
                           -> TargetSize
                           -> Word64 -- ^ .eh_frame address
                           -> Word64 -- ^ Current PC address
                           -> EhFrameHdrEnc -- ^ Encoding to use
                           -> Get Word64
getAdjustedEhFrameHdrValue  _ _ _ _ DW_EH_PE_omit =
  fail "Header value omitted."
getAdjustedEhFrameHdrValue end sz ehFrameAddr pcAddr enc = do
  -- Get initial value
  v <- getEhFrameHdrUnValue end sz enc
  -- Apply modifierEhFrameHdrEnc 0x0f
  case enc .&. 0x70 of
    DW_EH_PE_absptr -> pure $! v
    DW_EH_PE_pcrel   -> pure $! pcAddr + v
    DW_EH_PE_datarel -> pure $! ehFrameAddr + v
    a -> fail $ "Unsupported application " ++ show a

--------------------------------------------------------------------------------
-- Common information entries

-- | CIE personality data.
data CiePersonality
   = DirectCiePersonality !Word64
   | IndirectCiePersonality !Word64
   | NoCiePersonality
  deriving (Show)

-- | A Common Information Entry
data DW_CIE =
  DW_CIE
  { cieEndianess :: !Endianess
  , cieEncoding  :: !Encoding
  , cieSize      :: !Word64
    -- ^ Size of CIE after initial size field.
  , cieVersion               :: !Word8
  , cieAugmentation          :: !B.ByteString
  , cieAugmentationData      :: !B.ByteString
  , cieAddressSize           :: !(Maybe Word8)
  , cieSegmentDescSize       :: !(Maybe Word8)
  , cieCodeAlignmentFactor   :: !Word64
  , cieDataAlignmentFactor   :: !Int64
  , cieReturnAddressRegister :: !Word64
  , cieLangSpecDataEnc       :: !EhFrameHdrEnc
    -- ^ Language specific data encoding
  , ciePersonality :: !CiePersonality
  , cieFdeEncoding           :: !EhFrameHdrEnc
    -- ^ Encoding for FDE values
    --
    -- Note.  This is checked during parsing to ensure it is not an
    -- indirect encoding.
  , cieInitialInstructions   :: !B.ByteString
  } deriving (Show)

encodingSize  :: Encoding -> Int
encodingSize Encoding32 =  4
encodingSize Encoding64 = 12


frameTotalSize :: Encoding -> Word64 -> Word64
frameTotalSize enc sz = fromIntegral (encodingSize enc) + sz

{-
cieFDEs :: DW_CIE -> [Either String DW_FDE]
cieFDEs cie = V.toList $ fmap go (cieFDEBuffers cie)
  where go fdeBytes =
          case tryStrictGet (getFDE (cieEndianess cie) 0 0 cie) fdeBytes of
            Left  (_, msg) -> Left msg
            Right (_, _, fde) -> Right fde
-}

-- | Augmentation data from CIE
data CIEAugmentationData =
  CIEAugmentationData { cieAugCieByteCount :: !Word64
                        -- ^ Number of bytes of data in Cie (0 for no data).
                      , cieAugPersonality :: !CiePersonality
                        -- ^ Perdsonality data
                      , cieAugLangSpecDataEnc :: !EhFrameHdrEnc
                        -- ^ Use `DW_EH_PE_omit` if none.
                      , cieAugFdeEncoding :: !EhFrameHdrEnc
                        -- ^ FDE encoding
                      }

checkAugByteCount :: String -> Word64 -> Int64 -> Get ()
checkAugByteCount nm augByteCount augStartPos = do
  augEndPos <- Get.bytesRead
  when (augByteCount /= fromIntegral (augEndPos - augStartPos)) $ do
    fail $ "Incorrect " <> nm <> " augmentation byte count: " ++ show augByteCount

getCiePersonality :: Endianess
                  -> TargetSize
                  -> Get CiePersonality
getCiePersonality end sz = do
  perEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Personality encoding
  checkEncValid "Personality encoding" perEnc
  when ((perEnc .&. 0x70) /= DW_EH_PE_pcrel) $ do
    fail "Expected pcrel personality"
  per <- getEhFrameHdrUnValue end sz perEnc
  pure $! if (perEnc .&. DW_EH_PE_indirect) == 0 then
            DirectCiePersonality per
           else
            IndirectCiePersonality per

defaultLsdaEncoding :: EhFrameHdrEnc
defaultLsdaEncoding = DW_EH_PE_sdata4 .|.  DW_EH_PE_pcrel

processCieAugmentation :: Endianess
                       -> TargetSize
                       -> B.ByteString
                       -> Get CIEAugmentationData
processCieAugmentation _end _sz "" = do
  pure $! CIEAugmentationData { cieAugCieByteCount    = 0
                              , cieAugPersonality     = NoCiePersonality
                              , cieAugLangSpecDataEnc = DW_EH_PE_omit
                              , cieAugFdeEncoding     = DW_EH_PE_absptr
                              }
processCieAugmentation _end _sz "zR" = do
  augByteCount <- getULEB128 -- Augmentation byte count
  augStartPos <- Get.bytesRead
  fdeEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Get FDE encoding
  checkAugByteCount "CIE zR" augByteCount augStartPos
  pure $! CIEAugmentationData { cieAugCieByteCount    = augByteCount
                              , cieAugPersonality     = NoCiePersonality
                              , cieAugLangSpecDataEnc = defaultLsdaEncoding
                              , cieAugFdeEncoding     = fdeEnc
                              }
processCieAugmentation _end _sz "zRS" = do
  -- Note. I do not know what "s" means, so this is a copy of ZR.
  augByteCount <- getULEB128 -- Augmentation byte count
  augStartPos <- Get.bytesRead
  fdeEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Get FDE encoding
  checkAugByteCount "CIE zRS" augByteCount augStartPos
  pure $! CIEAugmentationData { cieAugCieByteCount    = augByteCount
                              , cieAugPersonality     = NoCiePersonality
                              , cieAugLangSpecDataEnc = defaultLsdaEncoding
                              , cieAugFdeEncoding     = fdeEnc
                              }
processCieAugmentation end sz "zPR" = do
  augByteCount <- getULEB128
  augStartPos <- Get.bytesRead
  per <- getCiePersonality end sz
  fdeEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Get fde encoding
  checkAugByteCount "CIE zPLR" augByteCount augStartPos
  pure $! CIEAugmentationData { cieAugCieByteCount    = augByteCount
                              , cieAugPersonality     = per
                              , cieAugLangSpecDataEnc = defaultLsdaEncoding
                              , cieAugFdeEncoding     = fdeEnc
                              }

processCieAugmentation end sz "zPLR" = do
  augByteCount <- getULEB128 -- Skip augmentation byte count
  augStartPos <- Get.bytesRead
  per <- getCiePersonality end sz

  lsdEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Get language specific data area
  checkEncValid "Language specific" lsdEnc
  when ((lsdEnc .&. DW_EH_PE_app) /= DW_EH_PE_pcrel) $ do
    fail $ "Expected language specific app to be PC relative: " ++ show lsdEnc

  fdeEnc <- EhFrameHdrEnc <$> Get.getWord8 -- Get fde encoding
  checkAugByteCount "CIE zPLR" augByteCount augStartPos
  pure $! CIEAugmentationData { cieAugCieByteCount    = augByteCount
                              , cieAugPersonality     = per
                              , cieAugLangSpecDataEnc = lsdEnc
                              , cieAugFdeEncoding     = fdeEnc
                              }
processCieAugmentation _end _sz aug = do
  fail $ "Unexpected augmentation: " ++ show aug

ppBytes :: B.ByteString -> String
ppBytes b = unwords (ppByte <$> B.unpack b)
  where ppByte w | w < 16 = "0" <> showHex w ""
                 | otherwise = showHex w ""

-- | Indicates if we are in a EhFrame or DebugFrame as the
-- parse rules are different
data FrameContext = EhFrame | DebugFrame

-- | Constant to distinguish CIE from FDE
cieConstant :: FrameContext -> Encoding -> Word64
cieConstant EhFrame _ = 0
cieConstant DebugFrame Encoding32 = fromIntegral (maxBound :: Word32)
cieConstant DebugFrame Encoding64 = maxBound

tgtByteCount :: TargetSize -> Word8
tgtByteCount TargetSize32 = 4
tgtByteCount TargetSize64 = 8

-- | Get the CIE except the fde entries
getCIE' :: FrameContext
        -> Endianess
        -> TargetSize
        -> B.ByteString
        -> Either (Int64, String) (B.ByteString, Int64, Maybe DW_CIE)
getCIE' ctx end tgtSize bytes = flip tryStrictGet bytes $ do
  (enc, size) <- getDwarfSize end
  if size == 0 then
    pure Nothing
   else do
    pos <- Get.bytesRead
    let endPos = pos + fromIntegral size
    cieId  <- desrGetOffset end enc
    when (cieId /= cieConstant ctx enc) $ do
      fail $ "CIE ID non-zero: " ++ ppBytes (B.take (fromIntegral (frameTotalSize enc size)) bytes)
    version               <- Get.getWord8
    when (version `notElem` [1,3,4]) $ do
      fail $ "Unrecognized CIE version " ++ show version <> "\n"
    augmentation          <- getByteStringNul
    when (version == 4) $ do
      addressSize <- Get.getWord8
      when (addressSize /= tgtByteCount tgtSize) $ do
        fail $ "Address size is unexpected."
      segmentSelectorSize <- Get.getWord8
      when (segmentSelectorSize /= 0) $ do
        fail "Segment descriptors not supported."
    codeAlignmentFactor   <- getULEB128
    dataAlignmentFactor   <- getSLEB128
    returnAddressRegister <-
      case version of
        1 -> fromIntegral <$> Get.getWord8
        _ -> getULEB128
    augData <- processCieAugmentation end tgtSize augmentation

    augEndPos <- fromIntegral <$> Get.bytesRead
    -- Get augmentation data excluding size.
    let augByteCount = cieAugCieByteCount augData
    let augDataBuffer :: B.ByteString
        augDataBuffer | augByteCount == 0 = B.empty
                      | otherwise =
                          B.drop (augEndPos - fromIntegral augByteCount) (B.take augEndPos bytes)

    let fdeEnc = cieAugFdeEncoding augData
    when (fdeEnc == DW_EH_PE_omit) $ do
      fail "Expected FDE encoding."
    when ((fdeEnc .&. DW_EH_PE_indirect) /= 0) $ do
      fail "Indirect FDE encodings not supported."

    instructions <- do
      curPos <- fromIntegral <$> Get.bytesRead
      Get.getByteString (fromIntegral (endPos - curPos))
    let cie = DW_CIE { cieEndianess = end
                     , cieEncoding  = enc
                     , cieSize      = size
                     , cieVersion               = version
                     , cieAugmentation          = augmentation
                     , cieAugmentationData      = augDataBuffer
                     , cieAddressSize = if version == 4 then Just (tgtByteCount tgtSize) else Nothing
                     , cieSegmentDescSize = if version == 4 then Just 0 else Nothing
                     , cieCodeAlignmentFactor   = codeAlignmentFactor
                     , cieDataAlignmentFactor   = dataAlignmentFactor
                     , cieReturnAddressRegister = returnAddressRegister
                     , cieLangSpecDataEnc = cieAugLangSpecDataEnc augData
                     , ciePersonality     = cieAugPersonality augData
                     , cieFdeEncoding     = fdeEnc
                     , cieInitialInstructions = instructions
                     }
    seq cie $ pure (Just cie)

-- | Get the CIE except the fde entries
getCIE :: FrameContext
       -> Endianess
       -> TargetSize
       -> B.ByteString -- ^ .eh_frame bytes
       -> Word64 -- ^ Offset within .eh_frame
       -> Either (Word64, String) (Word64, Maybe DW_CIE)
getCIE ctx end tgtSize ehFrame cieOff = do
  case getCIE' ctx end tgtSize (B.drop (fromIntegral cieOff) ehFrame) of
    Left (sz, msg) -> Left (cieOff + fromIntegral sz, msg)
    Right (_, sz, mcie) -> Right (cieOff + fromIntegral sz, mcie)

--------------------------------------------------------------------------------
-- Frame Description Entries

data DW_FDE =
  DW_FDE
  { fdeEncoding :: !Encoding
  , fdeSize :: !Word64
  , fdeCiePointer      :: !Word64
  , fdeStartAddress  :: !Word64
  , fdeAddressRange    :: !Word64
  , fdeLsdaAddress     :: !(Maybe Word64)
    -- ^ Language specific data area address.
  , fdeInstructions    :: !B.ByteString
  }
  deriving (Show)

getFDE' :: DW_CIE -- ^ CIE address
        -> Encoding -- ^ Encoding of FDE
        -> TargetSize -- ^ Address space size of taget
        -> Int -- ^ End index
        -> Word64 -- ^ Size
        -> Word64 -- CIE IDE
       -> Get DW_FDE
getFDE' cie enc tgtSize endPos size cieId = do
  let end = cieEndianess cie
  let fdeEnc = cieFdeEncoding cie
  let ehFrameAddr = error "ehFrameOff undefined"
  let pcAddr = 0 -- Note. Dwarfdump seems to use 0, but we should investigate this.
  initialLocation        <- getAdjustedEhFrameHdrValue end tgtSize ehFrameAddr pcAddr fdeEnc
  addressRange           <- getAdjustedEhFrameHdrValue end tgtSize ehFrameAddr pcAddr fdeEnc

  -- Get LSDA address if defined.
  mLsdaAddress <-
    if "z" `B.isPrefixOf` cieAugmentation cie then do
      augByteCount <- getULEB128
      if augByteCount > 0 then do
        augStartPos <- Get.bytesRead
        when (augByteCount /= 4) $ fail $ "Unexpected FDE augmentation byte count: " <> show augByteCount
        when (cieLangSpecDataEnc cie /= (DW_EH_PE_sdata4 .|.  DW_EH_PE_pcrel)) $ do
          fail $ "Unexpected aug lang spec data encoding: " <> show (cieLangSpecDataEnc cie, augByteCount)

        a <- fromIntegral <$> derGetI32 end -- getEhFrameHdrUnValue end tgtSize (cieLangSpecDataEnc cie)

        checkAugByteCount "FDE" augByteCount augStartPos
        pure $ Just a
       else
        pure Nothing
     else
      pure Nothing

  instructions <- do
    curPos <- fromIntegral <$> Get.bytesRead
    Get.getByteString $ fromIntegral (endPos - curPos)

  pure $! DW_FDE { fdeEncoding = enc
                 , fdeSize = size
                 , fdeCiePointer = cieId
                 , fdeStartAddress = initialLocation
                 , fdeAddressRange    = addressRange
                 , fdeLsdaAddress  = mLsdaAddress
                 , fdeInstructions = instructions
                 }

data FDEParseError
   = FDEParseError !Word64 !String
   | FDECIE !Word64
     -- ^ Encountered a common information entry at the offset in .eh_frame
   | FDEEnd !Word64
     -- ^ Reached end of .eh_frame section by encountering a empty
     -- entry that started at the given offset.
   | FDEReachedEnd
     -- ^ Reacghed the end of the file.

-- | Get FDE at given offset, and return new offset.
getFDEAt :: FrameContext
         -> TargetSize
         -> B.ByteString -- ^ Bytes in .eh_frame
         -> DW_CIE -- ^ Common information entry information for this FDE
         -> Word64 -- ^ Offset of FDE within .eh_frame
         -> Either FDEParseError (DW_FDE, Word64)
getFDEAt ctx tgtSize ehFrame cie off = do
  let end = cieEndianess cie
  when (off == fromIntegral (B.length ehFrame)) $ do
    Left FDEReachedEnd
  when (off >= fromIntegral (B.length ehFrame)) $ do
    Left (FDEParseError off "Invalid address.")
  (off2, (fdeEnc, size)) <-
    case tryStrictGet (getDwarfSize end) (B.drop (fromIntegral off) ehFrame) of
      Left _ ->  Left $ FDEParseError off "Failed to get dwarf size."
      Right (_,sz,r) -> Right (off + fromIntegral sz, r)
  when (size == 0) $ Left $ FDEEnd off
  (cieIdSize, cieId) <-
    case tryStrictGet (desrGetOffset end fdeEnc) (B.drop (fromIntegral off2) ehFrame) of
      Left _ -> Left $ FDEParseError off "Failed to cie id."
      Right (_,sz,cieId) -> Right (fromIntegral sz, cieId)
  when (cieId == cieConstant ctx fdeEnc) $ Left $ FDECIE off
  let off3 :: Int
      off3 = fromIntegral off2 + cieIdSize
  let remSize :: Int
      remSize = fromIntegral size - cieIdSize
  case tryStrictGet (getFDE' cie fdeEnc tgtSize remSize size cieId) (B.drop (fromIntegral off3) ehFrame) of
      Left  (_, msg) -> Left $ FDEParseError off msg
      Right (_, _, r)   -> Right (r, off2 + fromIntegral size)

--------------------------------------------------------------------------------
-- .eh_frame_hdr contents

-- | Information from an .eh_frame_hdr
data EhFrameHdr =
  EhFrameHdr
  { ehfhTargetSize :: !TargetSize
    -- ^ Indicattes if this is 32 or 64-bit.
  , ehfhAddr :: !Word64
  , ehfhFramePtrEnc :: !EhFrameHdrEnc
  , ehfhFDECountEnc :: !EhFrameHdrEnc
  , ehfhTableEnc    :: !EhFrameHdrEnc
  , ehfhFramePtr :: !Word64
  , ehfhTableAddr :: !Word64
    -- ^ Address of start table in memory.
    --
    -- Needed to resolve memory address of entries.
  , ehfhTable    :: !B.ByteString
    -- ^ data with binary-search tree entries.
    --
    -- Note. The number of bytes in this must be a multiple of
    -- the number of bytes in a BST entry.
  } deriving (Show)

-- | Number of bytes in a BST entry.
ehfhBstValueSize :: EhFrameHdr -> Int
ehfhBstValueSize hdr = ehFrameHdrValueSize (ehfhTargetSize hdr) (ehfhTableEnc hdr)

-- | Number of entries in a Ehframe
ehfhFdeCount :: EhFrameHdr -> Word64
ehfhFdeCount hdr = fromIntegral $
  B.length (ehfhTable hdr) `quot` (2 * ehfhBstValueSize hdr)

-- | Check we have a direct encoding that we can read from
checkEncValid :: String -> EhFrameHdrEnc -> Get ()
checkEncValid nm enc =
  when (enc == DW_EH_PE_omit) $ do
    fail $ "Expected " ++ nm ++ " encoding."

-- | Check we have a direct encoding that we can read from
checkEncDirectAndValid :: String -> EhFrameHdrEnc -> Get ()
checkEncDirectAndValid nm enc = do
  checkEncValid nm enc
  when ((enc .&. DW_EH_PE_indirect) /= 0) $ do
    fail $ "Indirect " ++ nm ++ " encodings not supported."

getEhFrameHdr :: Endianess
              -> TargetSize
              -> Word64 -- ^ Address of ehFrame header
              -> B.ByteString
              -> Either (Get.ByteOffset, String)
                        (B.ByteString, Get.ByteOffset, EhFrameHdr)
getEhFrameHdr end tgtSize ehFrameAddr bs = flip tryStrictGet bs $ do
  version       <- Get.getWord8
  when (version /= 1) $ do
    fail "Expected 1 for .eh_frame_hdr version."

  framePtrEnc   <- EhFrameHdrEnc <$> Get.getWord8
  checkEncDirectAndValid "frame pointer" framePtrEnc

  fdeCountEnc   <- EhFrameHdrEnc <$> Get.getWord8
  tableEnc      <- EhFrameHdrEnc <$> Get.getWord8

  framePtr  <- getAdjustedEhFrameHdrValue end tgtSize ehFrameAddr (ehFrameAddr + 4) framePtrEnc
  cntOff <- fromIntegral <$> Get.bytesRead
  (tableAddr, table)  <-
    if fdeCountEnc == DW_EH_PE_omit then
      pure (ehFrameAddr + cntOff, B.empty)
     else do
      checkEncDirectAndValid "FDE count" fdeCountEnc

      cnt <- getAdjustedEhFrameHdrValue end tgtSize ehFrameAddr (ehFrameAddr + cntOff) fdeCountEnc
      tblOff <- fromIntegral <$> Get.bytesRead
      if cnt == 0 then do
        pure (ehFrameAddr + tblOff, B.empty)
       else do
        checkEncDirectAndValid "table encoding" tableEnc
        let sz = ehFrameHdrValueSize tgtSize tableEnc
        when (sz == 0) $ do
          fail "Could not determine eh_frame table size."
        table <- Get.getByteString (fromIntegral cnt * 2 * sz)
        pure (ehFrameAddr + tblOff, table)

  pure $! EhFrameHdr { ehfhTargetSize = tgtSize
                     , ehfhAddr     = ehFrameAddr
                     , ehfhFramePtrEnc = framePtrEnc
                     , ehfhFDECountEnc = fdeCountEnc
                     , ehfhTableEnc    = tableEnc
                     , ehfhFramePtr    = framePtr
                     , ehfhTableAddr   = tableAddr
                     , ehfhTable       = table
                     }


-- | Entry in header binary-search table
data EhFrameHdrBstEntry = EhFrameHdrBstEntry
  { ehfhEntryLoc :: !Word64
    -- ^ Code address for this entry.
  , ehfhEntryFDEAddr :: !Word64
    -- ^ Address of FDE entry
  }
  deriving (Show)

-- | Get BST entry at given offset.
--
-- Note. This assumes the index is less than the number of FDEs.
ehFrameHdrBstEntry :: Endianess -> EhFrameHdr -> Word64 -> EhFrameHdrBstEntry
ehFrameHdrBstEntry end hdr i = do
  let tgtSize = ehfhTargetSize hdr
  let hdrAddr = ehfhAddr hdr
  let sz =  fromIntegral (ehfhBstValueSize hdr)
  let off = 2 * sz * i
  let entryAddr = ehfhTableAddr hdr + off
  let getEntry = do
        -- Note. we should already know the encoding is valid.
        l <- getAdjustedEhFrameHdrValue end tgtSize hdrAddr entryAddr        (ehfhTableEnc hdr)
        a <- getAdjustedEhFrameHdrValue end tgtSize hdrAddr (entryAddr + sz) (ehfhTableEnc hdr)
        pure $! EhFrameHdrBstEntry { ehfhEntryLoc     = l
                                   , ehfhEntryFDEAddr = a
                                   }
  case tryStrictGet getEntry (B.drop (fromIntegral off) (ehfhTable hdr)) of
    Left _ -> error "Failed to parse entry."
    Right (_, _, r) -> r
