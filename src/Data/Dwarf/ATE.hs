{-# LANGUAGE PatternSynonyms #-}
module Data.Dwarf.ATE
  ( DW_ATE(..)
  , pattern DW_ATE_address
  , pattern DW_ATE_boolean
  , pattern DW_ATE_complex_float
  , pattern DW_ATE_float
  , pattern DW_ATE_signed
  , pattern DW_ATE_signed_char
  , pattern DW_ATE_unsigned
  , pattern DW_ATE_unsigned_char
  , pattern DW_ATE_imaginary_float
  , pattern DW_ATE_packed_decimal
  , pattern DW_ATE_numeric_string

  , pattern DW_ATE_edited
  , pattern DW_ATE_signed_fixed
  , pattern DW_ATE_unsigned_fixed
  , pattern DW_ATE_decimal_float
  , pattern DW_ATE_UTF
  , pattern DW_ATE_lo_user
  , pattern DW_ATE_high_user
  ) where


import           Data.Char (isSeparator)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Numeric (readHex, showHex)

newtype DW_ATE = DW_ATE { dwAteValue :: Word64 }
  deriving (Eq,Ord)

pattern DW_ATE_address :: DW_ATE
pattern DW_ATE_address = DW_ATE 0x01

pattern DW_ATE_boolean :: DW_ATE
pattern DW_ATE_boolean = DW_ATE 0x02

pattern DW_ATE_complex_float :: DW_ATE
pattern DW_ATE_complex_float = DW_ATE 0x03

pattern DW_ATE_float :: DW_ATE
pattern DW_ATE_float = DW_ATE 0x04

pattern DW_ATE_signed :: DW_ATE
pattern DW_ATE_signed = DW_ATE 0x05

pattern DW_ATE_signed_char :: DW_ATE
pattern DW_ATE_signed_char = DW_ATE 0x06

pattern DW_ATE_unsigned :: DW_ATE
pattern DW_ATE_unsigned = DW_ATE 0x07

pattern DW_ATE_unsigned_char :: DW_ATE
pattern DW_ATE_unsigned_char = DW_ATE 0x08

pattern DW_ATE_imaginary_float :: DW_ATE
pattern DW_ATE_imaginary_float = DW_ATE 0x09

pattern DW_ATE_packed_decimal :: DW_ATE
pattern DW_ATE_packed_decimal = DW_ATE 0x0a

pattern DW_ATE_numeric_string :: DW_ATE
pattern DW_ATE_numeric_string = DW_ATE 0x0b

pattern DW_ATE_edited :: DW_ATE
pattern DW_ATE_edited = DW_ATE 0x0c

pattern DW_ATE_signed_fixed :: DW_ATE
pattern DW_ATE_signed_fixed = DW_ATE 0x0d

pattern DW_ATE_unsigned_fixed :: DW_ATE
pattern DW_ATE_unsigned_fixed = DW_ATE 0x0e

pattern DW_ATE_decimal_float :: DW_ATE
pattern DW_ATE_decimal_float = DW_ATE 0x0f

pattern DW_ATE_UTF :: DW_ATE
pattern DW_ATE_UTF = DW_ATE 0x10

pattern DW_ATE_lo_user :: DW_ATE
pattern DW_ATE_lo_user = DW_ATE 0x80

pattern DW_ATE_high_user :: DW_ATE
pattern DW_ATE_high_user = DW_ATE 0xff

dwAteValueNamePairs :: [(DW_ATE, String)]
dwAteValueNamePairs =
  [ (,) DW_ATE_address         "DW_ATE_address"
  , (,) DW_ATE_boolean         "DW_ATE_boolean"
  , (,) DW_ATE_complex_float   "DW_ATE_complex_float"
  , (,) DW_ATE_float           "DW_ATE_float"
  , (,) DW_ATE_signed          "DW_ATE_signed"
  , (,) DW_ATE_signed_char     "DW_ATE_signed_char"
  , (,) DW_ATE_unsigned        "DW_ATE_unsigned"
  , (,) DW_ATE_unsigned_char   "DW_ATE_unsigned_char"
  , (,) DW_ATE_imaginary_float "DW_ATE_imaginary_float"
  , (,) DW_ATE_packed_decimal  "DW_ATE_packed_decimal"
  , (,) DW_ATE_numeric_string  "DW_ATE_numeric_string"
  , (,) DW_ATE_edited          "DW_ATE_edited"
  , (,) DW_ATE_signed_fixed    "DW_ATE_signed_fixed"
  , (,) DW_ATE_unsigned_fixed  "DW_ATE_unsigned_fixed"
  , (,) DW_ATE_decimal_float   "DW_ATE_decimal_float"
  , (,) DW_ATE_UTF             "DW_ATE_UTF"
  , (,) DW_ATE_lo_user         "DW_ATE_lo_user"
  , (,) DW_ATE_high_user       "DW_ATE_high_user"
  ]

dwAteValueNameMap :: Map DW_ATE String
dwAteValueNameMap = Map.fromList dwAteValueNamePairs

dwAteNameValueMap :: Map String DW_ATE
dwAteNameValueMap = Map.fromList [ (nm, val) | (val, nm) <- dwAteValueNamePairs ]

instance Show DW_ATE where
  showsPrec _ v s =
    case Map.lookup v dwAteValueNameMap of
      Just r -> r ++ s
      Nothing -> "0x" ++ showHex (dwAteValue v) s

instance Read DW_ATE where
  readsPrec _ s =
    let (p,r) = span (not . isSeparator) s
     in case Map.lookup p dwAteNameValueMap of
          Just v -> [(v,r)]
          Nothing ->
            let (sp,sr) = splitAt 2 p
             in if sp == "0x" || sp == "0X" then
                  case readHex sr of
                    [(v, "")] -> [(DW_ATE v,r)]
                    _ -> []
                 else
                  []
