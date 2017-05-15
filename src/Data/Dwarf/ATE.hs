module Data.Dwarf.ATE where

import Data.Word (Word64)

data DW_ATE
    = DW_ATE_address
    | DW_ATE_boolean
    | DW_ATE_complex_float
    | DW_ATE_float
    | DW_ATE_signed
    | DW_ATE_signed_char
    | DW_ATE_unsigned
    | DW_ATE_unsigned_char
    | DW_ATE_imaginary_float
    | DW_ATE_packed_decimal
    | DW_ATE_numeric_string
    | DW_ATE_edited
    | DW_ATE_signed_fixed
    | DW_ATE_unsigned_fixed
    | DW_ATE_decimal_float
    deriving (Eq, Ord, Read, Show)

get_dw_ate :: Word64 -> Maybe DW_ATE
get_dw_ate 0x01 = Just DW_ATE_address
get_dw_ate 0x02 = Just DW_ATE_boolean
get_dw_ate 0x03 = Just DW_ATE_complex_float
get_dw_ate 0x04 = Just DW_ATE_float
get_dw_ate 0x05 = Just DW_ATE_signed
get_dw_ate 0x06 = Just DW_ATE_signed_char
get_dw_ate 0x07 = Just DW_ATE_unsigned
get_dw_ate 0x08 = Just DW_ATE_unsigned_char
get_dw_ate 0x09 = Just DW_ATE_imaginary_float
get_dw_ate 0x0a = Just DW_ATE_packed_decimal
get_dw_ate 0x0b = Just DW_ATE_numeric_string
get_dw_ate 0x0c = Just DW_ATE_edited
get_dw_ate 0x0d = Just DW_ATE_signed_fixed
get_dw_ate 0x0e = Just DW_ATE_unsigned_fixed
get_dw_ate 0x0f = Just DW_ATE_decimal_float
get_dw_ate _    = Nothing

dw_ate :: Word64 -> DW_ATE
dw_ate n =
  case get_dw_ate n of
    Just r -> r
    Nothing -> error $ "Unrecognized DW_ATE encoding " ++ show n
