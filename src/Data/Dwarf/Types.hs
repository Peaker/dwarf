module Data.Dwarf.Types where

import Data.Word (Word64)
import Data.List (stripPrefix)

newtype DieID = DieID Word64
  deriving (Eq, Ord)

instance Show DieID where
  show (DieID x) = "DIE@" ++ show x

instance Read DieID where
  readsPrec p str =
    case stripPrefix "DIE@" str of
      Just rest -> [ (DieID x, y) | (x, y) <- readsPrec p rest ]
      Nothing   -> []

data DW_DS
    = DW_DS_unsigned
    | DW_DS_leading_overpunch
    | DW_DS_trailing_overpunch
    | DW_DS_leading_separate
    | DW_DS_trailing_separate
    deriving (Eq, Ord, Read, Show)
dw_ds :: Word64 -> DW_DS
dw_ds 0x01 = DW_DS_unsigned
dw_ds 0x02 = DW_DS_leading_overpunch
dw_ds 0x03 = DW_DS_trailing_overpunch
dw_ds 0x04 = DW_DS_leading_separate
dw_ds 0x05 = DW_DS_trailing_separate
dw_ds tag = error $ "Invalid DW_DS tag: " ++ show tag

data DW_END
    = DW_END_default
    | DW_END_big
    | DW_END_little
    deriving (Eq, Ord, Read, Show)
dw_end :: Word64 -> DW_END
dw_end 0x00 = DW_END_default
dw_end 0x01 = DW_END_big
dw_end 0x02 = DW_END_little
dw_end n = error $ "Unrecognized DW_END value " ++ show n

data DW_ACCESS
    = DW_ACCESS_public
    | DW_ACCESS_protected
    | DW_ACCESS_private
    deriving (Eq, Ord, Read, Show)
dw_access :: Word64 -> DW_ACCESS
dw_access 0x01 = DW_ACCESS_public
dw_access 0x02 = DW_ACCESS_protected
dw_access 0x03 = DW_ACCESS_private
dw_access tag = error $ "Invalid dw_access tag: " ++ show tag

data DW_VIS
    = DW_VIS_local
    | DW_VIS_exported
    | DW_VIS_qualified
    deriving (Eq, Ord, Read, Show)
dw_vis :: Word64 -> DW_VIS
dw_vis 0x01 = DW_VIS_local
dw_vis 0x02 = DW_VIS_exported
dw_vis 0x03 = DW_VIS_qualified
dw_vis tag = error $ "Invalid DW_VIS tag: " ++ show tag

data DW_VIRTUALITY
    = DW_VIRTUALITY_none
    | DW_VIRTUALITY_virtual
    | DW_VIRTUALITY_return_virtual
    deriving (Eq, Ord, Read, Show)
dw_virtuality :: Word64 -> DW_VIRTUALITY
dw_virtuality 0x00 = DW_VIRTUALITY_none
dw_virtuality 0x01 = DW_VIRTUALITY_virtual
dw_virtuality 0x02 = DW_VIRTUALITY_return_virtual
dw_virtuality tag = error $ "Invalid tag for DW_VIRTUALITY: " ++ show tag

data DW_LANG
    = DW_LANG_C89
    | DW_LANG_C
    | DW_LANG_Ada83
    | DW_LANG_C_plus_plus
    | DW_LANG_Cobol74
    | DW_LANG_Cobol85
    | DW_LANG_Fortran77
    | DW_LANG_Fortran90
    | DW_LANG_Pascal83
    | DW_LANG_Modula2
    | DW_LANG_Java
    | DW_LANG_C99
    | DW_LANG_Ada95
    | DW_LANG_Fortran95
    | DW_LANG_PLI
    | DW_LANG_ObjC
    | DW_LANG_ObjC_plus_plus
    | DW_LANG_UPC
    | DW_LANG_D
    | DW_LANG_User Int -- 0x8000..0xFFFF
    deriving (Eq, Ord, Read, Show)
dw_lang :: Word64 -> DW_LANG
dw_lang 0x0001 = DW_LANG_C89
dw_lang 0x0002 = DW_LANG_C
dw_lang 0x0003 = DW_LANG_Ada83
dw_lang 0x0004 = DW_LANG_C_plus_plus
dw_lang 0x0005 = DW_LANG_Cobol74
dw_lang 0x0006 = DW_LANG_Cobol85
dw_lang 0x0007 = DW_LANG_Fortran77
dw_lang 0x0008 = DW_LANG_Fortran90
dw_lang 0x0009 = DW_LANG_Pascal83
dw_lang 0x000a = DW_LANG_Modula2
dw_lang 0x000b = DW_LANG_Java
dw_lang 0x000c = DW_LANG_C99
dw_lang 0x000d = DW_LANG_Ada95
dw_lang 0x000e = DW_LANG_Fortran95
dw_lang 0x000f = DW_LANG_PLI
dw_lang 0x0010 = DW_LANG_ObjC
dw_lang 0x0011 = DW_LANG_ObjC_plus_plus
dw_lang 0x0012 = DW_LANG_UPC
dw_lang 0x0013 = DW_LANG_D
dw_lang n
  | 0x8000 <= n && n <= 0xffff =
    DW_LANG_User $ fromIntegral n
  | otherwise =
    error $ "Unrecognized DW_LANG " ++ show n

data DW_ID
    = DW_ID_case_sensitive
    | DW_ID_up_case
    | DW_ID_down_case
    | DW_ID_case_insensitive
    deriving (Eq, Ord, Read, Show)
dw_id :: Word64 -> DW_ID
dw_id 0x00 = DW_ID_case_sensitive
dw_id 0x01 = DW_ID_up_case
dw_id 0x02 = DW_ID_down_case
dw_id 0x03 = DW_ID_case_insensitive
dw_id n = error $ "Unrecognized DW_ID " ++ show n

data DW_CC
    = DW_CC_normal
    | DW_CC_program
    | DW_CC_nocall
    deriving (Eq, Ord, Read, Show)
dw_cc :: Word64 -> DW_CC
dw_cc 0x01 = DW_CC_normal
dw_cc 0x02 = DW_CC_program
dw_cc 0x03 = DW_CC_nocall
dw_cc n = error $ "Unrecognized calling convention " ++ show n

data DW_INL
    = DW_INL_not_inlined
    | DW_INL_inlined
    | DW_INL_declared_not_inlined
    | DW_INL_declared_inlined
    deriving (Eq, Ord, Read, Show)
dw_inl :: Word64 -> DW_INL
dw_inl 0x00 = DW_INL_not_inlined
dw_inl 0x01 = DW_INL_inlined
dw_inl 0x02 = DW_INL_declared_not_inlined
dw_inl 0x03 = DW_INL_declared_inlined
dw_inl n = error $ "Unrecognized DW_INL " ++ show n

data DW_ORD
    = DW_ORD_row_major
    | DW_ORD_col_major
    deriving (Eq, Ord, Read, Show)
dw_ord :: Word64 -> DW_ORD
dw_ord 0x00 = DW_ORD_row_major
dw_ord 0x01 = DW_ORD_col_major
dw_ord n = error $ "Unrecognized DW_ORD " ++ show n

data DW_DSC
    = DW_DSC_label
    | DW_DSC_range
    deriving (Eq, Ord, Read, Show)
dw_dsc :: Word64 -> DW_DSC
dw_dsc 0x00 = DW_DSC_label
dw_dsc 0x01 = DW_DSC_range
dw_dsc n = error $ "Unrecognized DW_DSC " ++ show n
