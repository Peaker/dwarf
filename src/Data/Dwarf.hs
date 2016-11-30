{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parses the DWARF 2 and DWARF 3 specifications at http://www.dwarfstd.org given
-- the debug sections in ByteString form.
module Data.Dwarf
  ( Encoding(..), Endianess(..), TargetSize(..)
  , Sections(..)
  , parseInfo
  , DieID(..), dieID, DIE(..), (!?)
  , DIERefs(..), DIEMap
  , Reader(..), drEndianess, drEncoding
  , desrGetOffset
  , parseAranges
  , parsePubnames
  , parsePubtypes
  , CUContext(..)
  , Range(..), parseRanges, parseLoc
  , RangeEnd(..)
  , DW_CFA(..)
  , DW_MACINFO(..), parseMacInfo
  , DW_CIEFDE(..), parseFrame
  , DW_OP(..), parseDW_OP, parseDW_OPs, getDW_OP
  , module Data.Dwarf.AT
  , module Data.Dwarf.TAG
  , module Data.Dwarf.Types
  , DW_LNE(..), parseLNE, getLNE
  , DW_ATE(..), dw_ate, get_dw_ate
  , DW_DS(..)
  , DW_END(..)
  , DW_ACCESS(..)
  , DW_VIS(..)
  , DW_VIRTUALITY(..)
  , DW_ID(..)
  , DW_INL(..)
  , DW_CC(..)
  , DW_ORD(..)
  , DW_DSC(..)
  ) where

import Control.Arrow ((&&&), (***))
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
    -- ^ ".debug_info section"
  , dsAbbrevSection :: B.ByteString
  , dsStrSection :: B.ByteString
  }

data CUContext = CUContext
  { cuOffset :: CUOffset
    -- ^ Offset .debug_info section
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
      (fmap . map) (DW_AT *** dw_form) . whileM (/= (0,0)) $
      (,) <$> getULEB128 <*> getULEB128


---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- DWARF information entry and .debug_info section parsing.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Utility function for retrieving the list of values for a specified attribute from a DWARF information entry.
(!?) :: DIE -> DW_AT -> [DW_ATVAL]
(!?) die at = map snd $ filter ((== at) . fst) $ dieAttributes die

getNonZeroOffset :: Endianess -> Encoding -> Get (Maybe Word64)
getNonZeroOffset end enc = do
  offset <- desrGetOffset end enc
  pure $ if offset == 0 then Nothing else Just offset

-- Section 7.19 - Name Lookup Tables
getNameLookupEntries :: Endianess -> Encoding -> CUOffset -> Get [(String, [DieID])]
getNameLookupEntries end enc cu_offset = do
  whileJust $ traverse getEntry =<< getNonZeroOffset end enc
  where
    getEntry die_offset = do
      name <- getUTF8Str0
      pure (name, [inCU cu_offset die_offset])

-- The headers for "Section 7.19 Name Lookup Table", and "Section 7.20
-- Address Range Table" are very similar, this is the common format:
getTableHeader :: Endianess -> Get (Encoding, CUOffset)
getTableHeader endianess = do
  (enc, _) <- getDwarfSize endianess
  _version <- derGetW16 endianess
  cu_offset <- desrGetOffset endianess enc
  return (enc, CUOffset cu_offset)

getNameLookupTable :: Endianess -> Get [M.Map String [DieID]]
getNameLookupTable endianess = getWhileNotEmpty $ do
  (enc, cu_offset) <- getTableHeader endianess
  _debug_info_length <- desrGetOffset endianess enc
  M.fromListWith (++) <$> getNameLookupEntries endianess enc cu_offset

parsePubSection :: Endianess -> B.ByteString -> M.Map String [DieID]
parsePubSection endianess section =
  M.unionsWith (++) $ strictGet (getNameLookupTable endianess) section

-- | Parses the .debug_pubnames section (as ByteString) into a map from a value name to a DieID
parsePubnames :: Endianess -> B.ByteString -> M.Map String [DieID]
parsePubnames = parsePubSection

-- | Parses the .debug_pubtypes section (as ByteString) into a map from a type name to a DieID
parsePubtypes :: Endianess -> B.ByteString -> M.Map String [DieID]
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
getAddressRangeTable :: Endianess -> Get [([Range], CUOffset)]
getAddressRangeTable endianess = getWhileNotEmpty $ do
  (_enc, cu_offset)   <- getTableHeader endianess
  address_size      <- getWord8
  let
    readAddress =
      case address_size of
        4 -> fromIntegral <$> derGetW32 endianess
        8 -> derGetW64 endianess
        n -> fail $ "Unrecognized address size " ++ show n ++ " in .debug_aranges section."
  _segment_size     <- getWord8
  align $ 2 * address_size
  address_ranges <- whileM (/= Range 0 0) $ Range <$> readAddress <*> readAddress
  pure (address_ranges, cu_offset)

-- | Parses the .debug_aranges section (as ByteString) into a map from
-- an address range to a DieID that indexes the Info.
parseAranges :: Endianess -> B.ByteString -> [([Range], CUOffset)]
parseAranges endianess aranges_section =
  strictGet (getAddressRangeTable endianess) aranges_section

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
getCIEFDE end target64 = do
    (enc, size) <- getDwarfSize end
    pos <- Get.bytesRead
    let endPos = fromIntegral pos + size
    cie_id     <- desrGetOffset end enc
    if cie_id == encodingLargestOffset enc then do
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
        let initial_instructions =
              strictGet (getWhileNotEmpty (getDW_CFA end target64)) raw_instructions
        pure $ DW_CIE augmentation code_alignment_factor data_alignment_factor return_address_register initial_instructions
     else do
        initial_location        <- getTargetAddress end target64
        address_range           <- getTargetAddress end target64
        curPos                  <- fromIntegral <$> Get.bytesRead
        raw_instructions        <- getByteString $ fromIntegral (endPos - curPos)
        let instructions = strictGet (getWhileNotEmpty (getDW_CFA end target64)) raw_instructions
        pure $ DW_FDE cie_id initial_location address_range instructions

-- | Parse the .debug_frame section into a list of DW_CIEFDE records.
parseFrame
  :: Endianess
  -> TargetSize
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

getMRange :: Endianess -> TargetSize -> Get (Maybe (Either RangeEnd Range))
getMRange endian tgt = do
  begin <- getTargetAddress endian tgt
  end   <- getTargetAddress endian tgt
  pure $
    if begin == 0 && end == 0
    then Nothing
    else Just $
      if begin == largestTargetAddress tgt
      then Left $ RangeEnd end
      else Right $ Range begin end

getRanges :: Reader -> Get [Either RangeEnd Range]
getRanges dr = whileJust $ getMRange (drEndianess dr) (drTarget64 dr)

-- Section 7.7.3
-- | Retrieves the location list expressions from a given substring of the .debug_loc section. The offset
-- into the .debug_loc section is obtained from an attribute of class loclistptr for a given DIE.
-- Left results are base address entries. Right results are address ranges and a location expression.
parseLoc :: Endianess -> TargetSize -> B.ByteString -> [Either RangeEnd (Range, B.ByteString)]
parseLoc endian tgt = strictGet (getLoc endian tgt)

getLoc :: Endianess -> TargetSize -> Get [Either RangeEnd (Range, B.ByteString)]
getLoc endian tgt = whileJust $ traverse mkRange =<< getMRange endian tgt
  where
    mkRange (Left end) = pure $ Left end
    mkRange (Right range) =
      Right . (,) range <$> getByteStringLen (derGetW16 endian)

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

withToldRefs :: (Applicative m, Monad m) => Maybe DieID -> [DIE] -> DIECollector m ()
withToldRefs mParent dies =
  Writer.tell $ M.fromList $ map (dieId . dieRefsDIE &&& id) $ addRefs mParent dies

-- Decode a non-compilation unit DWARF information entry, its children and its siblings.
getDieAndSiblings :: DieID -> CUContext -> DIECollector Get [DIE]
getDieAndSiblings parent cuContext = do
  dies <- (whileJust . getDIEAndDescendants) cuContext
  withToldRefs (Just parent) dies
  pure dies

getForm :: CUContext -> DW_FORM -> Get DW_ATVAL
getForm cuContext@CUContext { cuReader = dr, cuOffset = cu, cuSections = dc } form =
 let end = drEndianess dr
     enc = drEncoding dr
     tgt = drTarget64 dr
   in case form of
    DW_FORM_addr         -> DW_ATVAL_UINT . fromIntegral <$> getTargetAddress end tgt
    DW_FORM_block1       -> DW_ATVAL_BLOB <$> getByteStringLen getWord8
    DW_FORM_block2       -> DW_ATVAL_BLOB <$> getByteStringLen (derGetW16 end)
    DW_FORM_block4       -> DW_ATVAL_BLOB <$> getByteStringLen (derGetW32 end)
    DW_FORM_block        -> DW_ATVAL_BLOB <$> getByteStringLen getULEB128
    DW_FORM_data1        -> DW_ATVAL_UINT . fromIntegral <$> getWord8
    DW_FORM_data2        -> DW_ATVAL_UINT . fromIntegral <$> derGetW16 end
    DW_FORM_data4        -> DW_ATVAL_UINT . fromIntegral <$> derGetW32 end
    DW_FORM_data8        -> DW_ATVAL_UINT . fromIntegral <$> derGetW64 end
    DW_FORM_udata        -> DW_ATVAL_UINT <$> getULEB128
    DW_FORM_sdata        -> DW_ATVAL_INT <$> getSLEB128
    DW_FORM_flag         -> DW_ATVAL_BOOL . (/= 0) <$> getWord8
    DW_FORM_string       -> DW_ATVAL_STRING <$> getUTF8Str0
    DW_FORM_ref1         -> DW_ATVAL_REF . inCU cu <$> getWord8
    DW_FORM_ref2         -> DW_ATVAL_REF . inCU cu <$> derGetW16 end
    DW_FORM_ref4         -> DW_ATVAL_REF . inCU cu <$> derGetW32 end
    DW_FORM_ref8         -> DW_ATVAL_REF . inCU cu <$> derGetW64 end
    DW_FORM_ref_udata    -> DW_ATVAL_REF . inCU cu <$> getULEB128
    DW_FORM_ref_addr     -> DW_ATVAL_UINT <$> desrGetOffset end enc
    DW_FORM_sec_offset   -> DW_ATVAL_UINT <$> desrGetOffset end enc
    DW_FORM_exprloc      -> DW_ATVAL_BLOB <$> getByteStringLen getULEB128
    DW_FORM_flag_present -> pure $ DW_ATVAL_BOOL True
    DW_FORM_ref_sig8     -> DW_ATVAL_UINT . fromIntegral <$> derGetW64 end
    DW_FORM_indirect     -> getForm cuContext . dw_form =<< getULEB128
    DW_FORM_strp         -> do
      offset <- fromIntegral <$> desrGetOffset end (drEncoding dr)
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

getCUHeader :: Endianess
            -> Sections
            -> Get CUContext
getCUHeader endian dwarfSections = do
  cu_offset       <- CUOffset . fromIntegral <$> Get.bytesRead

  (enc, _)        <- getDwarfSize endian
  _version        <- derGetW16 endian
  abbrev_offset   <- desrGetOffset endian enc
  let abbrev_map   = M.fromList . map (abbrevId &&& id) .
                     getAt getAbbrevList abbrev_offset $
                     dsAbbrevSection dwarfSections
  addr_size       <- getWord8
  tgt  <- case addr_size of
            4 -> pure $ TargetSize32
            8 -> pure $ TargetSize64
            _ -> fail $ "Invalid address size: " ++ show addr_size
  let ctx = CUContext { cuReader = reader endian enc tgt
                      , cuAbbrevMap = abbrev_map
                      , cuOffset = cu_offset
                      , cuSections = dwarfSections
                      }
  return ctx

-- TODO: Why not return CUs rather than DIE's?
-- Decode the compilation unit DWARF information entries.
getDieCus :: Endianess -> Sections -> DIECollector Get [(CUContext,DIE)]
getDieCus endianess dwarfSections = do
  let go l = do
        e <- lift Get.isEmpty
        if e then
          pure $ reverse l
         else do
          ctx <- lift $ getCUHeader endianess dwarfSections
          mr <- getDIEAndDescendants ctx
          case mr of
            Nothing -> fail "Compilation Unit must have a DIE"
            Just r -> go ((ctx,r) : l)
  dies <- go []
  withToldRefs Nothing (snd <$> dies)
  pure dies

-- | Parses the .debug_info section (as ByteString) using the .debug_abbrev and .debug_str sections.
parseInfo :: Endianess -> Sections -> ([(CUContext,DIE)], DIEMap)  -- ^ The die list is of compilation unit dies
parseInfo endianess dwarfSections = strictGet act $ dsInfoSection dwarfSections
  where act = runWriterT $ getDieCus endianess dwarfSections
