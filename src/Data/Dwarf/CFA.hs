module Data.Dwarf.CFA where

import Control.Applicative (Applicative(..))
import Data.Binary.Get (getWord8, Get)
import Data.Bits (shiftR, (.&.))
import Data.Dwarf.Reader
import Data.Dwarf.Utils
import Data.Int (Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B

-- Section 7.22 - Call Frame
data DW_CFA
    = DW_CFA_advance_loc Word8
    | DW_CFA_offset Word8 Word64
    | DW_CFA_restore Word8
    | DW_CFA_nop
    | DW_CFA_set_loc Word64
    | DW_CFA_advance_loc1 Word8
    | DW_CFA_advance_loc2 Word16
    | DW_CFA_advance_loc4 Word32
    | DW_CFA_offset_extended Word64 Word64
    | DW_CFA_restore_extended Word64
    | DW_CFA_undefined Word64
    | DW_CFA_same_value Word64
    | DW_CFA_register Word64 Word64
    | DW_CFA_remember_state
    | DW_CFA_restore_state
    | DW_CFA_def_cfa Word64 Word64
    | DW_CFA_def_cfa_register Word64
    | DW_CFA_def_cfa_offset Word64
    | DW_CFA_def_cfa_expression B.ByteString
    | DW_CFA_expression Word64 B.ByteString
    | DW_CFA_offset_extended_sf Word64 Int64
    | DW_CFA_def_cfa_sf Word64 Int64
    | DW_CFA_def_cfa_offset_sf Int64
    | DW_CFA_val_offset Word64 Word64
    | DW_CFA_val_offset_sf Word64 Int64
    | DW_CFA_val_expression Word64 B.ByteString
    deriving (Eq, Ord, Read, Show)

getDW_CFA :: Endianess -> TargetSize -> Get DW_CFA
getDW_CFA end tgt = do
    tag <- getWord8
    case tag `shiftR` 6 of
        0x1 -> pure $ DW_CFA_advance_loc $ tag .&. 0x3f
        0x2 -> pure (DW_CFA_offset (tag .&. 0x3f)) <*> getULEB128
        0x3 -> pure $ DW_CFA_restore $ tag .&. 0x3f
        0x0 -> case tag .&. 0x3f of
            0x00 -> pure DW_CFA_nop
            0x01 -> pure DW_CFA_set_loc <*> getTargetAddress end tgt
            0x02 -> pure DW_CFA_advance_loc1 <*> getWord8
            0x03 -> pure DW_CFA_advance_loc2 <*> derGetW16 end
            0x04 -> pure DW_CFA_advance_loc4 <*> derGetW32 end
            0x05 -> pure DW_CFA_offset_extended <*> getULEB128 <*> getULEB128
            0x06 -> pure DW_CFA_restore_extended <*> getULEB128
            0x07 -> pure DW_CFA_undefined <*> getULEB128
            0x08 -> pure DW_CFA_same_value <*> getULEB128
            0x09 -> pure DW_CFA_register <*> getULEB128 <*> getULEB128
            0x0a -> pure DW_CFA_remember_state
            0x0b -> pure DW_CFA_restore_state
            0x0c -> pure DW_CFA_def_cfa <*> getULEB128 <*> getULEB128
            0x0d -> pure DW_CFA_def_cfa_register <*> getULEB128
            0x0e -> pure DW_CFA_def_cfa_offset <*> getULEB128
            0x0f -> pure DW_CFA_def_cfa_expression <*> getByteStringLen getULEB128
            0x10 -> pure DW_CFA_expression <*> getULEB128 <*> getByteStringLen getULEB128
            0x11 -> pure DW_CFA_offset_extended_sf <*> getULEB128 <*> getSLEB128
            0x12 -> pure DW_CFA_def_cfa_sf <*> getULEB128 <*> getSLEB128
            0x13 -> pure DW_CFA_def_cfa_offset_sf <*> getSLEB128
            0x14 -> pure DW_CFA_val_offset <*> getULEB128 <*> getULEB128
            0x15 -> pure DW_CFA_val_offset_sf <*> getULEB128 <*> getSLEB128
            0x16 -> pure DW_CFA_val_expression <*> getULEB128 <*> getByteStringLen getULEB128
            _ -> fail $ "Invalid tag: " ++ show tag
        _ -> fail $ "Invalid tag: " ++ show tag
