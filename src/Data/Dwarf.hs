{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parses the DWARF 2 and DWARF 3 specifications at http://www.dwarfstd.org given
-- the debug sections in ByteString form.
module Data.Dwarf
  ( Endianess(..), TargetSize(..)
  , Sections(..)
  , parseInfo
  , DieID, dieID, DIE(..), (!?)
  , DIERefs(..), DIEMap
  , Reader(..)
  , parseAranges
  , parsePubnames
  , parsePubtypes
  , Range(..), parseRanges, parseLoc
  , RangeEnd(..)
  , DW_CFA(..)
  , DW_MACINFO(..), parseMacInfo
  , DW_CIEFDE(..), parseFrame
  , DW_OP(..), parseDW_OP, parseDW_OPs
  , DW_TAG(..)
  , DW_AT(..)
  , DW_ATVAL(..)
  , DW_LNE(..), parseLNE
  , DW_ATE(..), dw_ate
  , DW_DS(..), dw_ds
  , DW_END(..), dw_end
  , DW_ACCESS(..), dw_access
  , DW_VIS(..), dw_vis
  , DW_VIRTUALITY(..), dw_virtuality
  , DW_LANG(..), dw_lang
  , DW_ID(..), dw_id
  , DW_INL(..), dw_inl
  , DW_CC(..), dw_cc
  , DW_ORD(..), dw_ord
  , DW_DSC(..), dw_dsc
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((&&&), (***))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Binary (Get)
import Data.Binary.Get (getWord8, getByteString)
import Data.Dwarf.AT
import Data.Dwarf.ATE
import Data.Dwarf.Form
import Data.Dwarf.LNI
import Data.Dwarf.OP
import Data.Dwarf.TAG
import Data.Dwarf.CFA
import Data.Dwarf.Reader
import Data.Dwarf.Types
import Data.Dwarf.Utils
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Traversable (traverse)
import Data.Word (Word64)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as B
import qualified Data.Map as M

newtype CUOffset = CUOffset Word64
  deriving (Eq, Ord, Read, Show)

-- Don't export a constructor, so users can only read DieID's, not
-- create fake ones, which is slightly safer.
dieID :: DieID -> Word64
dieID (DieID x) = x

inCU :: Integral a => CUOffset -> a -> DieID
inCU (CUOffset base) x = DieID $ base + fromIntegral x

data Sections = Sections
  { dsInfoSection :: B.ByteString
  , dsAbbrevSection :: B.ByteString
  , dsStrSection :: B.ByteString
  }

data CUContext = CUContext
  { cuOffset :: CUOffset
  , cuAbbrevMap :: M.Map AbbrevId DW_ABBREV
  , cuReader :: Reader
  , cuSections :: Sections
  }

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Abbreviation and form parsing
---------------------------------------------------------------------------------------------------------------------------------------------------------------
newtype AbbrevId = AbbrevId Word64
  deriving (Eq, Ord, Read, Show)

data DW_ABBREV = DW_ABBREV
    { abbrevId        :: AbbrevId
    , abbrevTag       :: DW_TAG
    , abbrevChildren  :: Bool
    , abbrevAttrForms :: [(DW_AT, DW_FORM)]
    }

getMAbbrevId :: Get (Maybe AbbrevId)
getMAbbrevId = do
  i <- getULEB128
  pure $
    if i == 0
    then Nothing
    else Just $ AbbrevId i

getAbbrevList :: Get [DW_ABBREV]
getAbbrevList =
  whileJust $ traverse getAbbrev =<< getMAbbrevId
  where
    getAbbrev abbrev = do
      tag       <- getDW_TAG
      children  <- (== 1) <$> getWord8
      attrForms <- getAttrFormList
      pure $ DW_ABBREV abbrev tag children attrForms
    getAttrFormList =
      (fmap . map) (dw_at *** dw_form) . whileM (/= (0,0)) $
      (,) <$> getULEB128 <*> getULEB128


---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- DWARF information entry and .debug_info section parsing.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Utility function for retrieving the list of values for a specified attribute from a DWARF information entry.
(!?) :: DIE -> DW_AT -> [DW_ATVAL]
(!?) die at = map snd $ filter ((== at) . fst) $ dieAttributes die

getNonZeroOffset :: Reader -> Get (Maybe Word64)
getNonZeroOffset dr = do
  offset <- drGetOffset dr
  pure $ if offset == 0 then Nothing else Just offset

-- Section 7.19 - Name Lookup Tables
getNameLookupEntries :: Reader -> CUOffset -> Get [(String, [DieID])]
getNameLookupEntries dr cu_offset =
  whileJust $ traverse getEntry =<< getNonZeroOffset dr
  where
    getEntry die_offset = do
      name <- getUTF8Str0
      pure (name, [inCU cu_offset die_offset])

-- The headers for "Section 7.19 Name Lookup Table", and "Section 7.20
-- Address Range Table" are very similar, this is the common format:
getTableHeader :: TargetSize -> EndianReader -> Get (Reader, CUOffset)
getTableHeader target64 der = do
  (desr, _) <- getUnitLength der
  let dr = reader target64 desr
  _version <- drGetW16 dr
  cu_offset <- drGetOffset dr
  return (dr, CUOffset cu_offset)

getNameLookupTable :: TargetSize -> EndianReader -> Get [M.Map String [DieID]]
getNameLookupTable target64 der = getWhileNotEmpty $ do
  (dr, cu_offset) <- getTableHeader target64 der
  _debug_info_length <- drGetOffset dr
  M.fromListWith (++) <$> getNameLookupEntries dr cu_offset

parsePubSection :: Endianess -> TargetSize -> B.ByteString -> M.Map String [DieID]
parsePubSection endianess target64 section =
  M.unionsWith (++) $ strictGet (getNameLookupTable target64 der) section
  where
    der = endianReader endianess

-- | Parses the .debug_pubnames section (as ByteString) into a map from a value name to a DieID
parsePubnames :: Endianess -> TargetSize -> B.ByteString -> M.Map String [DieID]
parsePubnames = parsePubSection

-- | Parses the .debug_pubtypes section (as ByteString) into a map from a type name to a DieID
parsePubtypes :: Endianess -> TargetSize -> B.ByteString -> M.Map String [DieID]
parsePubtypes = parsePubSection

align :: Integral a => a -> Get ()
align alignment = do
  pos <- Get.bytesRead
  Get.skip . fromIntegral $ (-pos) `mod` fromIntegral alignment

data Range = Range
  { rangeBegin :: !Word64
  , rangeEnd :: !Word64
  } deriving (Eq, Ord, Read, Show)

-- Section 7.20 - Address Range Table
-- Returns the ranges that belong to a CU
getAddressRangeTable :: TargetSize -> EndianReader -> Get [([Range], CUOffset)]
getAddressRangeTable target64 der = getWhileNotEmpty $ do
  (dr, cu_offset)   <- getTableHeader target64 der
  address_size      <- getWord8
  let
    readAddress =
      case address_size of
        4 -> fromIntegral <$> drGetW32 dr
        8 -> drGetW64 dr
        n -> fail $ "Unrecognized address size " ++ show n ++ " in .debug_aranges section."
  _segment_size     <- getWord8
  align $ 2 * address_size
  address_ranges <- whileM (/= Range 0 0) $ Range <$> readAddress <*> readAddress
  pure (address_ranges, cu_offset)

-- | Parses the .debug_aranges section (as ByteString) into a map from
-- an address range to a DieID that indexes the Info.
parseAranges ::
  Endianess -> TargetSize -> B.ByteString -> [([Range], CUOffset)]
parseAranges endianess target64 aranges_section =
    let dr = endianReader endianess
    in strictGet (getAddressRangeTable target64 dr) aranges_section

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Section 7.21 - Macro Information
data DW_MACINFO
    = DW_MACINFO_define Word64 String     -- ^ Line number and defined symbol with definition
    | DW_MACINFO_undef Word64 String      -- ^ Line number and undefined symbol
    | DW_MACINFO_start_file Word64 Word64 -- ^ Marks start of file with the line where the file was included from and a source file index
    | DW_MACINFO_end_file                 -- ^ Marks end of file
    | DW_MACINFO_vendor_ext Word64 String -- ^ Implementation defined
    deriving (Eq, Ord, Read, Show)

-- | Retrieves the macro information for a compilation unit from a given substring of the .debug_macinfo section. The offset
-- into the .debug_macinfo section is obtained from the DW_AT_macro_info attribute of a compilation unit DIE.
parseMacInfo :: B.ByteString -> [DW_MACINFO]
parseMacInfo = strictGet getMacInfo

getMacInfo :: Get [DW_MACINFO]
getMacInfo = do
    x <- getWord8
    case x of
        0x00 -> pure []
        0x01 -> pure (:) <*> (pure DW_MACINFO_define     <*> getULEB128 <*> getUTF8Str0) <*> getMacInfo
        0x02 -> pure (:) <*> (pure DW_MACINFO_undef      <*> getULEB128 <*> getUTF8Str0) <*> getMacInfo
        0x03 -> pure (:) <*> (pure DW_MACINFO_start_file <*> getULEB128 <*> getULEB128)  <*> getMacInfo
        0x04 -> pure (:) <*>  pure DW_MACINFO_end_file                                   <*> getMacInfo
        0xff -> pure (:) <*> (pure DW_MACINFO_vendor_ext <*> getULEB128 <*> getUTF8Str0) <*> getMacInfo
        _ -> fail $ "Invalid MACINFO id: " ++ show x

data DW_CIEFDE
    = DW_CIE
        { cieAugmentation          :: String
        , cieCodeAlignmentFactor   :: Word64
        , cieDataAlignmentFactor   :: Int64
        , cieReturnAddressRegister :: Word64
        , cieInitialInstructions   :: [DW_CFA]
        }
    | DW_FDE
        { fdeCiePointer      :: Word64
        , fdeInitialLocation :: Word64
        , fdeAddressRange    :: Word64
        , fdeInstructions    :: [DW_CFA]
        }
    deriving (Eq, Ord, Read, Show)

getCIEFDE :: Endianess -> TargetSize -> Get DW_CIEFDE
getCIEFDE endianess target64 = do
    let der    = endianReader endianess
    (desr, endPos) <- getUnitLength der
    let dr     = reader target64 desr
    cie_id     <- drGetOffset dr
    if cie_id == drLargestOffset dr then do
        version                 <- getWord8
        augmentation            <- getUTF8Str0
        code_alignment_factor   <- getULEB128
        data_alignment_factor   <- getSLEB128
        return_address_register <- case version of
                                    1 -> fromIntegral <$> getWord8
                                    3 -> getULEB128
                                    n -> fail $ "Unrecognized CIE version " ++ show n
        curPos                  <- fromIntegral <$> Get.bytesRead
        raw_instructions        <- getByteString $ fromIntegral (endPos - curPos)
        let initial_instructions = strictGet (getWhileNotEmpty (getDW_CFA dr)) raw_instructions
        pure $ DW_CIE augmentation code_alignment_factor data_alignment_factor return_address_register initial_instructions
     else do
        initial_location        <- drGetTargetAddress dr
        address_range           <- drGetTargetAddress dr
        curPos                  <- fromIntegral <$> Get.bytesRead
        raw_instructions        <- getByteString $ fromIntegral (endPos - curPos)
        let instructions        = strictGet (getWhileNotEmpty (getDW_CFA dr)) raw_instructions
        pure $ DW_FDE cie_id initial_location address_range instructions

-- | Parse the .debug_frame section into a list of DW_CIEFDE records.
parseFrame ::
  Endianess -> TargetSize
  -> B.ByteString -- ^ ByteString for the .debug_frame section.
  -> [DW_CIEFDE]
parseFrame endianess target64 =
  strictGet . getWhileNotEmpty $ getCIEFDE endianess target64

newtype RangeEnd = RangeEnd Word64
                   deriving Show

-- Section 7.23 - Non-contiguous Address Ranges
-- | Retrieves the non-contiguous address ranges for a compilation unit from a given substring of the .debug_ranges section. The offset
-- into the .debug_ranges section is obtained from the DW_AT_ranges attribute of a compilation unit DIE.
-- Left results are base address entries. Right results are address ranges.
parseRanges :: Reader -> B.ByteString -> [Either RangeEnd Range]
parseRanges = strictGet . getRanges

getMRange :: Reader -> Get (Maybe (Either RangeEnd Range))
getMRange dr = do
  begin <- drGetTargetAddress dr
  end   <- drGetTargetAddress dr
  pure $
    if begin == 0 && end == 0
    then Nothing
    else Just $
      if begin == drLargestTargetAddress dr
      then Left $ RangeEnd end
      else Right $ Range begin end

getRanges :: Reader -> Get [Either RangeEnd Range]
getRanges dr = whileJust $ getMRange dr

-- Section 7.7.3
-- | Retrieves the location list expressions from a given substring of the .debug_loc section. The offset
-- into the .debug_loc section is obtained from an attribute of class loclistptr for a given DIE.
-- Left results are base address entries. Right results are address ranges and a location expression.
parseLoc :: Reader -> B.ByteString -> [Either RangeEnd (Range, B.ByteString)]
parseLoc dr = strictGet (getLoc dr)

getLoc :: Reader -> Get [Either RangeEnd (Range, B.ByteString)]
getLoc dr = whileJust $ traverse mkRange =<< getMRange dr
  where
    mkRange (Left end) = pure $ Left end
    mkRange (Right range) =
      Right . (,) range <$> getByteStringLen (drGetW16 dr)

data DIERefs = DIERefs
  { dieRefsParent       :: Maybe DieID   -- ^ Unique identifier of this entry's parent.
  , dieRefsSiblingLeft  :: Maybe DieID   -- ^ Unique identifier of the left sibling
  , dieRefsSiblingRight :: Maybe DieID   -- ^ Unique identifier of the right sibling
  , dieRefsDIE :: DIE
  } deriving (Show)

type DIEMap = M.Map DieID DIERefs
type DIECollector = WriterT DIEMap

-- | The dwarf information entries form a graph of nodes tagged with attributes. Please refer to the DWARF specification
-- for semantics. Although it looks like a tree, there can be attributes which have adjacency information which will
-- introduce cross-branch edges.
data DIE = DIE
    { dieId         :: DieID              -- ^ Unique identifier for this entry.
    , dieTag        :: DW_TAG              -- ^ Type tag.
    , dieAttributes :: [(DW_AT, DW_ATVAL)] -- ^ Attribute tag and value pairs.
    , dieChildren   :: [DIE]
    , dieReader     :: Reader         -- ^ Decoder used to decode this entry. May be needed to further parse attribute values.
    }
instance Show DIE where
  show (DIE (DieID i) tag attrs children _) =
    concat $ ["DIE@", show i, "{", show tag, " (", show (length children), " children)"] ++ concat
    [ [" ", show attr, "=(", show val, ")"]
    | (attr, val) <- attrs
    ] ++ ["}"]

addRefs :: Maybe DieID -> [DIE] -> [DIERefs]
addRefs mParent = go Nothing
  where
    go _lSibling [] = []
    go lSibling (die : xs) =
      DIERefs mParent lSibling (dieId <$> listToMaybe xs) die :
      go (Just (dieId die)) xs

withToldRefs :: (Applicative m, Monad m) => Maybe DieID -> [DIE] -> DIECollector m [DIE]
withToldRefs mParent dies =
  dies <$
  (Writer.tell . M.fromList . map (dieId . dieRefsDIE &&& id) . addRefs mParent) dies

-- Decode a non-compilation unit DWARF information entry, its children and its siblings.
getDieAndSiblings :: DieID -> CUContext -> DIECollector Get [DIE]
getDieAndSiblings parent cuContext =
  withToldRefs (Just parent) =<< (whileJust . getDIEAndDescendants) cuContext

getForm :: CUContext -> DW_FORM -> Get DW_ATVAL
getForm
  cuContext@CUContext { cuReader = dr, cuOffset = cu, cuSections = dc }
  form
  = case form of
    DW_FORM_addr         -> DW_ATVAL_UINT . fromIntegral <$> drGetTargetAddress dr
    DW_FORM_block1       -> DW_ATVAL_BLOB <$> getByteStringLen getWord8
    DW_FORM_block2       -> DW_ATVAL_BLOB <$> getByteStringLen (drGetW16 dr)
    DW_FORM_block4       -> DW_ATVAL_BLOB <$> getByteStringLen (drGetW32 dr)
    DW_FORM_block        -> DW_ATVAL_BLOB <$> getByteStringLen getULEB128
    DW_FORM_data1        -> DW_ATVAL_UINT . fromIntegral <$> getWord8
    DW_FORM_data2        -> DW_ATVAL_UINT . fromIntegral <$> drGetW16 dr
    DW_FORM_data4        -> DW_ATVAL_UINT . fromIntegral <$> drGetW32 dr
    DW_FORM_data8        -> DW_ATVAL_UINT . fromIntegral <$> drGetW64 dr
    DW_FORM_udata        -> DW_ATVAL_UINT <$> getULEB128
    DW_FORM_sdata        -> DW_ATVAL_INT <$> getSLEB128
    DW_FORM_flag         -> DW_ATVAL_BOOL . (/= 0) <$> getWord8
    DW_FORM_string       -> DW_ATVAL_STRING <$> getUTF8Str0
    DW_FORM_ref1         -> DW_ATVAL_REF . inCU cu <$> getWord8
    DW_FORM_ref2         -> DW_ATVAL_REF . inCU cu <$> drGetW16 dr
    DW_FORM_ref4         -> DW_ATVAL_REF . inCU cu <$> drGetW32 dr
    DW_FORM_ref8         -> DW_ATVAL_REF . inCU cu <$> drGetW64 dr
    DW_FORM_ref_udata    -> DW_ATVAL_REF . inCU cu <$> getULEB128
    DW_FORM_ref_addr     -> DW_ATVAL_UINT <$> drGetOffset dr
    DW_FORM_sec_offset   -> DW_ATVAL_UINT <$> drGetOffset dr
    DW_FORM_exprloc      -> DW_ATVAL_BLOB <$> getByteStringLen getULEB128
    DW_FORM_flag_present -> pure $ DW_ATVAL_BOOL True
    DW_FORM_ref_sig8     -> DW_ATVAL_UINT . fromIntegral <$> drGetW64 dr
    DW_FORM_indirect     -> getForm cuContext . dw_form =<< getULEB128
    DW_FORM_strp         -> do
      offset <- fromIntegral <$> drGetOffset dr
      pure . DW_ATVAL_STRING .
        getAt getUTF8Str0 offset $ dsStrSection dc

getDIEAndDescendants :: CUContext -> DIECollector Get (Maybe DIE)
getDIEAndDescendants cuContext = do
  offset <- lift $ DieID . fromIntegral <$> Get.bytesRead
  let
    go abbrid = do
      let
        abbrev         = cuAbbrevMap cuContext M.! abbrid
        tag            = abbrevTag abbrev
        (attrs, forms) = unzip $ abbrevAttrForms abbrev
      values          <- lift $ mapM (getForm cuContext) forms
      children <-
        if abbrevChildren abbrev
        then getDieAndSiblings offset cuContext
        else pure []
      pure $ DIE offset tag (zip attrs values) children dr
  traverse go =<< lift getMAbbrevId
  where
    dr = cuReader cuContext

getCUHeader ::
  EndianReader -> Sections ->
  Get (CUOffset, M.Map AbbrevId DW_ABBREV, Reader)
getCUHeader der dwarfSections = do
  cu_offset       <- CUOffset . fromIntegral <$> Get.bytesRead
  (desr, _)       <- getUnitLength der
  _version        <- desrGetW16 desr
  abbrev_offset   <- desrGetOffset desr
  let abbrev_map   = M.fromList . map (abbrevId &&& id) .
                     getAt getAbbrevList abbrev_offset $
                     dsAbbrevSection dwarfSections
  addr_size       <- getWord8
  dr              <- case addr_size of
                      4 -> pure $ reader TargetSize32 desr
                      8 -> pure $ reader TargetSize64 desr
                      _ -> fail $ "Invalid address size: " ++ show addr_size
  return (cu_offset, abbrev_map, dr)

-- TODO: Why not return CUs rather than DIE's?
-- Decode the compilation unit DWARF information entries.
getDieCus :: EndianReader -> Sections -> DIECollector Get [DIE]
getDieCus der dwarfSections =
  withToldRefs Nothing <=<
  whileJust . condAct (lift Get.isEmpty) $ do
    (cu_offset, abbrev_map, dr) <- lift $ getCUHeader der dwarfSections
    maybe (fail "Compilation Unit must have a DIE") return =<<
      getDIEAndDescendants CUContext
        { cuReader = dr
        , cuAbbrevMap = abbrev_map
        , cuOffset = cu_offset
        , cuSections = dwarfSections
        }

-- | Parses the .debug_info section (as ByteString) using the .debug_abbrev and .debug_str sections.
parseInfo :: Endianess -> Sections -> ([DIE], DIEMap)  -- ^ The die list is of compilation unit dies
parseInfo endianess dwarfSections =
  strictGet act $ dsInfoSection dwarfSections
  where
    act = runWriterT $ getDieCus dr dwarfSections
    dr = endianReader endianess
