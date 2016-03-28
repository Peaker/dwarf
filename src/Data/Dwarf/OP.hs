module Data.Dwarf.OP where

import Control.Applicative (Applicative(..), (<$>))
import Data.Binary.Get (getWord8, Get, getByteString)
import Data.Dwarf.Reader
import Data.Dwarf.Utils
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B

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
    deriving (Eq, Ord, Read, Show)
             
-- | Parse a ByteString into a DWARF opcode. This will be needed for further decoding of DIE attributes.
parseDW_OP :: Reader -> B.ByteString -> DW_OP
parseDW_OP = strictGet . getDW_OP

-- | Parse a ByteString into a DWARF opcode expression (sequence of ops).
parseDW_OPs :: Reader -> B.ByteString -> [DW_OP]
parseDW_OPs = strictGet . getWhileNotEmpty . getDW_OP

getDW_OP :: Reader -> Get DW_OP
getDW_OP dr = getWord8 >>= getDW_OP_
  where
    getDW_OP_ :: Word8 -> Get DW_OP
    getDW_OP_ 0x03 = pure DW_OP_addr <*> drGetTargetAddress dr
    getDW_OP_ 0x06 = pure DW_OP_deref
    getDW_OP_ 0x08 = pure DW_OP_const1u <*> fromIntegral <$> getWord8
    getDW_OP_ 0x09 = pure DW_OP_const1s <*> fromIntegral <$> getWord8
    getDW_OP_ 0x0a = pure DW_OP_const2u <*> fromIntegral <$> drGetW16 dr
    getDW_OP_ 0x0b = pure DW_OP_const2s <*> fromIntegral <$> drGetW16 dr
    getDW_OP_ 0x0c = pure DW_OP_const4u <*> fromIntegral <$> drGetW32 dr
    getDW_OP_ 0x0d = pure DW_OP_const4s <*> fromIntegral <$> drGetW32 dr
    getDW_OP_ 0x0e = pure DW_OP_const8u <*> drGetW64 dr
    getDW_OP_ 0x0f = pure DW_OP_const8s <*> fromIntegral <$> drGetW64 dr
    getDW_OP_ 0x10 = pure DW_OP_constu  <*> getULEB128
    getDW_OP_ 0x11 = pure DW_OP_consts  <*> getSLEB128
    getDW_OP_ 0x12 = pure DW_OP_dup
    getDW_OP_ 0x13 = pure DW_OP_drop
    getDW_OP_ 0x14 = pure DW_OP_over
    getDW_OP_ 0x15 = pure DW_OP_pick <*> getWord8
    getDW_OP_ 0x16 = pure DW_OP_swap
    getDW_OP_ 0x17 = pure DW_OP_rot
    getDW_OP_ 0x18 = pure DW_OP_xderef
    getDW_OP_ 0x19 = pure DW_OP_abs
    getDW_OP_ 0x1a = pure DW_OP_and
    getDW_OP_ 0x1b = pure DW_OP_div
    getDW_OP_ 0x1c = pure DW_OP_minus
    getDW_OP_ 0x1d = pure DW_OP_mod
    getDW_OP_ 0x1e = pure DW_OP_mul
    getDW_OP_ 0x1f = pure DW_OP_neg
    getDW_OP_ 0x20 = pure DW_OP_not
    getDW_OP_ 0x21 = pure DW_OP_or
    getDW_OP_ 0x22 = pure DW_OP_plus
    getDW_OP_ 0x23 = pure DW_OP_plus_uconst <*> getULEB128
    getDW_OP_ 0x24 = pure DW_OP_shl
    getDW_OP_ 0x25 = pure DW_OP_shr
    getDW_OP_ 0x26 = pure DW_OP_shra
    getDW_OP_ 0x27 = pure DW_OP_xor
    getDW_OP_ 0x2f = pure DW_OP_skip <*> fromIntegral <$> drGetW16 dr
    getDW_OP_ 0x28 = pure DW_OP_bra  <*> fromIntegral <$> drGetW16 dr
    getDW_OP_ 0x29 = pure DW_OP_eq
    getDW_OP_ 0x2a = pure DW_OP_ge
    getDW_OP_ 0x2b = pure DW_OP_gt
    getDW_OP_ 0x2c = pure DW_OP_le
    getDW_OP_ 0x2d = pure DW_OP_lt
    getDW_OP_ 0x2e = pure DW_OP_ne
    getDW_OP_ 0x30 = pure $ DW_OP_lit 0
    getDW_OP_ 0x31 = pure $ DW_OP_lit 1
    getDW_OP_ 0x32 = pure $ DW_OP_lit 2
    getDW_OP_ 0x33 = pure $ DW_OP_lit 3
    getDW_OP_ 0x34 = pure $ DW_OP_lit 4
    getDW_OP_ 0x35 = pure $ DW_OP_lit 5
    getDW_OP_ 0x36 = pure $ DW_OP_lit 6
    getDW_OP_ 0x37 = pure $ DW_OP_lit 7
    getDW_OP_ 0x38 = pure $ DW_OP_lit 8
    getDW_OP_ 0x39 = pure $ DW_OP_lit 9
    getDW_OP_ 0x3a = pure $ DW_OP_lit 10
    getDW_OP_ 0x3b = pure $ DW_OP_lit 11
    getDW_OP_ 0x3c = pure $ DW_OP_lit 12
    getDW_OP_ 0x3d = pure $ DW_OP_lit 13
    getDW_OP_ 0x3e = pure $ DW_OP_lit 14
    getDW_OP_ 0x3f = pure $ DW_OP_lit 15
    getDW_OP_ 0x40 = pure $ DW_OP_lit 16
    getDW_OP_ 0x41 = pure $ DW_OP_lit 17
    getDW_OP_ 0x42 = pure $ DW_OP_lit 18
    getDW_OP_ 0x43 = pure $ DW_OP_lit 19
    getDW_OP_ 0x44 = pure $ DW_OP_lit 20
    getDW_OP_ 0x45 = pure $ DW_OP_lit 21
    getDW_OP_ 0x46 = pure $ DW_OP_lit 22
    getDW_OP_ 0x47 = pure $ DW_OP_lit 23
    getDW_OP_ 0x48 = pure $ DW_OP_lit 24
    getDW_OP_ 0x49 = pure $ DW_OP_lit 25
    getDW_OP_ 0x4a = pure $ DW_OP_lit 26
    getDW_OP_ 0x4b = pure $ DW_OP_lit 27
    getDW_OP_ 0x4c = pure $ DW_OP_lit 28
    getDW_OP_ 0x4d = pure $ DW_OP_lit 29
    getDW_OP_ 0x4e = pure $ DW_OP_lit 30
    getDW_OP_ 0x4f = pure $ DW_OP_lit 31
    getDW_OP_ 0x50 = pure $ DW_OP_reg 0
    getDW_OP_ 0x51 = pure $ DW_OP_reg 1
    getDW_OP_ 0x52 = pure $ DW_OP_reg 2
    getDW_OP_ 0x53 = pure $ DW_OP_reg 3
    getDW_OP_ 0x54 = pure $ DW_OP_reg 4
    getDW_OP_ 0x55 = pure $ DW_OP_reg 5
    getDW_OP_ 0x56 = pure $ DW_OP_reg 6
    getDW_OP_ 0x57 = pure $ DW_OP_reg 7
    getDW_OP_ 0x58 = pure $ DW_OP_reg 8
    getDW_OP_ 0x59 = pure $ DW_OP_reg 9
    getDW_OP_ 0x5a = pure $ DW_OP_reg 10
    getDW_OP_ 0x5b = pure $ DW_OP_reg 11
    getDW_OP_ 0x5c = pure $ DW_OP_reg 12
    getDW_OP_ 0x5d = pure $ DW_OP_reg 13
    getDW_OP_ 0x5e = pure $ DW_OP_reg 14
    getDW_OP_ 0x5f = pure $ DW_OP_reg 15
    getDW_OP_ 0x60 = pure $ DW_OP_reg 16
    getDW_OP_ 0x61 = pure $ DW_OP_reg 17
    getDW_OP_ 0x62 = pure $ DW_OP_reg 18
    getDW_OP_ 0x63 = pure $ DW_OP_reg 19
    getDW_OP_ 0x64 = pure $ DW_OP_reg 20
    getDW_OP_ 0x65 = pure $ DW_OP_reg 21
    getDW_OP_ 0x66 = pure $ DW_OP_reg 22
    getDW_OP_ 0x67 = pure $ DW_OP_reg 23
    getDW_OP_ 0x68 = pure $ DW_OP_reg 24
    getDW_OP_ 0x69 = pure $ DW_OP_reg 25
    getDW_OP_ 0x6a = pure $ DW_OP_reg 26
    getDW_OP_ 0x6b = pure $ DW_OP_reg 27
    getDW_OP_ 0x6c = pure $ DW_OP_reg 28
    getDW_OP_ 0x6d = pure $ DW_OP_reg 29
    getDW_OP_ 0x6e = pure $ DW_OP_reg 30
    getDW_OP_ 0x6f = pure $ DW_OP_reg 31
    getDW_OP_ 0x70 = pure (DW_OP_breg 0)  <*> getSLEB128
    getDW_OP_ 0x71 = pure (DW_OP_breg 1)  <*> getSLEB128
    getDW_OP_ 0x72 = pure (DW_OP_breg 2)  <*> getSLEB128
    getDW_OP_ 0x73 = pure (DW_OP_breg 3)  <*> getSLEB128
    getDW_OP_ 0x74 = pure (DW_OP_breg 4)  <*> getSLEB128
    getDW_OP_ 0x75 = pure (DW_OP_breg 5)  <*> getSLEB128
    getDW_OP_ 0x76 = pure (DW_OP_breg 6)  <*> getSLEB128
    getDW_OP_ 0x77 = pure (DW_OP_breg 7)  <*> getSLEB128
    getDW_OP_ 0x78 = pure (DW_OP_breg 8)  <*> getSLEB128
    getDW_OP_ 0x79 = pure (DW_OP_breg 9)  <*> getSLEB128
    getDW_OP_ 0x7a = pure (DW_OP_breg 10) <*> getSLEB128
    getDW_OP_ 0x7b = pure (DW_OP_breg 11) <*> getSLEB128
    getDW_OP_ 0x7c = pure (DW_OP_breg 12) <*> getSLEB128
    getDW_OP_ 0x7d = pure (DW_OP_breg 13) <*> getSLEB128
    getDW_OP_ 0x7e = pure (DW_OP_breg 14) <*> getSLEB128
    getDW_OP_ 0x7f = pure (DW_OP_breg 15) <*> getSLEB128
    getDW_OP_ 0x80 = pure (DW_OP_breg 16) <*> getSLEB128
    getDW_OP_ 0x81 = pure (DW_OP_breg 17) <*> getSLEB128
    getDW_OP_ 0x82 = pure (DW_OP_breg 18) <*> getSLEB128
    getDW_OP_ 0x83 = pure (DW_OP_breg 19) <*> getSLEB128
    getDW_OP_ 0x84 = pure (DW_OP_breg 20) <*> getSLEB128
    getDW_OP_ 0x85 = pure (DW_OP_breg 21) <*> getSLEB128
    getDW_OP_ 0x86 = pure (DW_OP_breg 22) <*> getSLEB128
    getDW_OP_ 0x87 = pure (DW_OP_breg 23) <*> getSLEB128
    getDW_OP_ 0x88 = pure (DW_OP_breg 24) <*> getSLEB128
    getDW_OP_ 0x89 = pure (DW_OP_breg 25) <*> getSLEB128
    getDW_OP_ 0x8a = pure (DW_OP_breg 26) <*> getSLEB128
    getDW_OP_ 0x8b = pure (DW_OP_breg 27) <*> getSLEB128
    getDW_OP_ 0x8c = pure (DW_OP_breg 28) <*> getSLEB128
    getDW_OP_ 0x8d = pure (DW_OP_breg 29) <*> getSLEB128
    getDW_OP_ 0x8e = pure (DW_OP_breg 30) <*> getSLEB128
    getDW_OP_ 0x8f = pure (DW_OP_breg 31) <*> getSLEB128
    getDW_OP_ 0x90 = pure DW_OP_regx   <*> getULEB128
    getDW_OP_ 0x91 = pure DW_OP_fbreg  <*> getSLEB128
    getDW_OP_ 0x92 = pure DW_OP_bregx  <*> getULEB128 <*> getSLEB128
    getDW_OP_ 0x93 = pure DW_OP_piece  <*> getULEB128
    getDW_OP_ 0x94 = pure DW_OP_deref_size <*> getWord8
    getDW_OP_ 0x95 = pure DW_OP_xderef_size <*> getWord8
    getDW_OP_ 0x96 = pure DW_OP_nop
    getDW_OP_ 0x97 = pure DW_OP_push_object_address
    getDW_OP_ 0x98 = pure DW_OP_call2 <*> drGetW16 dr
    getDW_OP_ 0x99 = pure DW_OP_call4 <*> drGetW32 dr
    getDW_OP_ 0x9a = pure DW_OP_call_ref <*> drGetTargetAddress dr
    getDW_OP_ 0x9b = pure DW_OP_form_tls_address
    getDW_OP_ 0x9c = pure DW_OP_call_frame_cfa
    getDW_OP_ 0x9d = pure DW_OP_bit_piece <*> getULEB128 <*> getULEB128
    getDW_OP_ 0x9e = do len <- getULEB128
                        pure DW_OP_implicit_value <*> getByteString (fromIntegral len)
    getDW_OP_ 0x9f = pure DW_OP_stack_value

    getDW_OP_ n | 0xe0 <= n && n <= 0xff = fail $ "User DW_OP data requires extension of parser for code " ++ show n
    getDW_OP_ n = fail $ "Unrecognized DW_OP code " ++ show n
