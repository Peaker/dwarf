module Data.Dwarf.OP where

import           Data.Binary.Get (getWord8, Get, getByteString, isolate)
import qualified Data.ByteString as B
import           Data.Dwarf.Reader
import           Data.Dwarf.Utils
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Numeric (showHex)

data DW_OP
    = DW_OP_addr Word64
    | DW_OP_deref
    | DW_OP_const1u Word8
    | DW_OP_const1s Int8
    | DW_OP_const2u Word16
    | DW_OP_const2s Int16
    | DW_OP_const4u Word32
    | DW_OP_const4s Int32
    | DW_OP_const8u Word64
    | DW_OP_const8s Int64
    | DW_OP_constu  Word64
    | DW_OP_consts  Int64
    | DW_OP_dup
    | DW_OP_drop
    | DW_OP_over
    | DW_OP_pick Word8
    | DW_OP_swap
    | DW_OP_rot
    | DW_OP_xderef
    | DW_OP_abs
    | DW_OP_and
    | DW_OP_div
    | DW_OP_minus
    | DW_OP_mod
    | DW_OP_mul
    | DW_OP_neg
    | DW_OP_not
    | DW_OP_or
    | DW_OP_plus
    | DW_OP_plus_uconst Word64
    | DW_OP_shl
    | DW_OP_shr
    | DW_OP_shra
    | DW_OP_xor
    | DW_OP_skip Int16
    | DW_OP_bra Int16
    | DW_OP_eq
    | DW_OP_ge
    | DW_OP_gt
    | DW_OP_le
    | DW_OP_lt
    | DW_OP_ne
    | DW_OP_lit Int
    | DW_OP_reg Int
    | DW_OP_breg Int Int64
    | DW_OP_regx Word64
    | DW_OP_fbreg Int64
    | DW_OP_bregx Word64 Int64
    | DW_OP_piece Word64
    | DW_OP_deref_size Word8
    | DW_OP_xderef_size Word8
    | DW_OP_nop
    | DW_OP_push_object_address
    | DW_OP_call2 Word16
    | DW_OP_call4 Word32
    | DW_OP_call_ref Word64
    | DW_OP_form_tls_address
    | DW_OP_call_frame_cfa
    | DW_OP_bit_piece Word64 Word64
    | DW_OP_implicit_value B.ByteString -- FIXME: maybe this could be Word64 etc.?
    | DW_OP_stack_value
    | DW_OP_GNU_implicit_pointer !Word64 !Int64
      -- ^ See 'http://www.dwarfstd.org/doc/040408.1.html'
    | DW_OP_GNU_entry_value [DW_OP]
      -- ^ See 'http://www.dwarfstd.org/doc/040408.1.html'
    | DW_OP_GNU_regval_type !Word64 !Word64
      -- ^ See 'http://www.dwarfstd.org/doc/040408.1.html'
    | DW_OP_GNU_convert !Word64
      -- ^ See 'http://www.dwarfstd.org/doc/040408.1.html'
    deriving (Eq, Ord, Read, Show)

-- | Parse a ByteString into a DWARF opcode. This will be needed for further decoding of DIE attributes.
parseDW_OP :: Reader -> B.ByteString -> DW_OP
parseDW_OP dr = strictGet (getDW_OP dr)

-- | Parse a ByteString into a DWARF opcode expression (sequence of ops).
parseDW_OPs :: Reader -> B.ByteString -> [DW_OP]
parseDW_OPs dr bs = strictGet (getWhileNotEmpty (getDW_OP dr)) bs

getDW_OP :: Reader -> Get DW_OP
getDW_OP dr = do
  let end = drEndianess dr
  let enc = drEncoding  dr
  let tgt = drTarget64  dr
  w <- getWord8
  case w of
    0x03 -> pure DW_OP_addr <*> getTargetAddress end tgt
    0x06 -> pure DW_OP_deref
    0x08 -> pure DW_OP_const1u <*> fromIntegral <$> getWord8
    0x09 -> pure DW_OP_const1s <*> fromIntegral <$> getWord8
    0x0a -> pure DW_OP_const2u <*> fromIntegral <$> derGetW16 end
    0x0b -> pure DW_OP_const2s <*> fromIntegral <$> derGetW16 end
    0x0c -> pure DW_OP_const4u <*> fromIntegral <$> derGetW32 end
    0x0d -> pure DW_OP_const4s <*> fromIntegral <$> derGetW32 end
    0x0e -> pure DW_OP_const8u <*> derGetW64 end
    0x0f -> pure DW_OP_const8s <*> fromIntegral <$> derGetW64 end
    0x10 -> pure DW_OP_constu  <*> getULEB128
    0x11 -> pure DW_OP_consts  <*> getSLEB128
    0x12 -> pure DW_OP_dup
    0x13 -> pure DW_OP_drop
    0x14 -> pure DW_OP_over
    0x15 -> DW_OP_pick <$> getWord8
    0x16 -> pure DW_OP_swap
    0x17 -> pure DW_OP_rot
    0x18 -> pure DW_OP_xderef
    0x19 -> pure DW_OP_abs
    0x1a -> pure DW_OP_and
    0x1b -> pure DW_OP_div
    0x1c -> pure DW_OP_minus
    0x1d -> pure DW_OP_mod
    0x1e -> pure DW_OP_mul
    0x1f -> pure DW_OP_neg
    0x20 -> pure DW_OP_not
    0x21 -> pure DW_OP_or
    0x22 -> pure DW_OP_plus
    0x23 -> DW_OP_plus_uconst <$> getULEB128
    0x24 -> pure DW_OP_shl
    0x25 -> pure DW_OP_shr
    0x26 -> pure DW_OP_shra
    0x27 -> pure DW_OP_xor
    0x2f -> (DW_OP_skip . fromIntegral) <$> derGetW16 end
    0x28 -> (DW_OP_bra  . fromIntegral) <$> derGetW16 end
    0x29 -> pure DW_OP_eq
    0x2a -> pure DW_OP_ge
    0x2b -> pure DW_OP_gt
    0x2c -> pure DW_OP_le
    0x2d -> pure DW_OP_lt
    0x2e -> pure DW_OP_ne
    _ | 0x30 <= w && w <= 0x4f -> pure $ DW_OP_lit (fromIntegral (w - 0x30))
    _ | 0x50 <= w && w <= 0x6f -> pure $ DW_OP_reg (fromIntegral (w - 0x50))
    _ | 0x70 <= w && w <= 0x8f -> DW_OP_breg (fromIntegral (w - 0x70)) <$> getSLEB128

    0x90 -> DW_OP_regx        <$> getULEB128
    0x91 -> DW_OP_fbreg       <$> getSLEB128
    0x92 -> DW_OP_bregx       <$> getULEB128 <*> getSLEB128
    0x93 -> DW_OP_piece       <$> getULEB128
    0x94 -> DW_OP_deref_size  <$> getWord8
    0x95 -> DW_OP_xderef_size <$> getWord8
    0x96 -> pure DW_OP_nop
    0x97 -> pure DW_OP_push_object_address
    0x98 -> DW_OP_call2    <$> derGetW16 end
    0x99 -> DW_OP_call4    <$> derGetW32 end
    0x9a -> DW_OP_call_ref <$> getTargetAddress end tgt
    0x9b -> pure DW_OP_form_tls_address
    0x9c -> pure DW_OP_call_frame_cfa
    0x9d -> DW_OP_bit_piece <$> getULEB128 <*> getULEB128
    0x9e -> do len <- getULEB128
               DW_OP_implicit_value <$> getByteString (fromIntegral len)
    0x9f -> pure DW_OP_stack_value

    0xf2 -> DW_OP_GNU_implicit_pointer <$> desrGetOffset end enc <*> getSLEB128
    0xf3 -> do len <- getULEB128
               DW_OP_GNU_entry_value
                 <$> isolate (fromIntegral len) (getWhileNotEmpty (getDW_OP dr))
    0xf5 -> DW_OP_GNU_regval_type <$> getULEB128 <*> getULEB128
    0xf7 -> DW_OP_GNU_convert     <$> getULEB128

    _ | 0xe0 <= w && w <= 0xff ->
        fail $ "User DW_OP data requires extension of parser for code " ++ showHex w ""
      | otherwise ->
        fail $ "Unrecognized DW_OP code " ++ show w
