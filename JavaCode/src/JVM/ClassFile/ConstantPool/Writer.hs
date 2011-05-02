-- | Writer for Constant Pool
module Java.VM.Class.File.ConstantPool.Writer (

    cpoolWrite,
    writeCPoolIndex,
    writeCPoolIndex8

) where {

    import Java.VM.Class.File.IO.WriterUtil;
    import Java.VM.Class.File.ConstantPool.Model;
    import Data.Word(Word8,Word16);
    import qualified Data.Map as Map;

    writeCPoolIndex :: CPoolIndex -> OutputValue;
    writeCPoolIndex (CPoolIndex i) = UI16 i;

    writeCPoolIndex8 :: CPoolIndex -> OutputValue;
    writeCPoolIndex8 (CPoolIndex i) = UI8 $ fromIntegral i;

    cpoolWrite :: ConstantPool -> OutputValue;
    cpoolWrite cp@(ConstantPool _ entries _) = 
        let (cpOut,slots) = writeCPEntries $ Map.elems entries
         in OutputValues [ UI16 (slots + 1), cpOut ];
                                    
    -- | Write array of cp entries and return output and number of slots used                                    
    writeCPEntries :: [CPEntry] -> (OutputValue,Word16);
    writeCPEntries [] = (OutputNothing, 0);
    writeCPEntries (e:es) = let (restOut,slots) = writeCPEntries es in    
        (OutputValues [case e of {
            (CPClass       i     ) -> OutputValues [ UI8 cpClassInfoTag      , UI16 (deIdx i) ];
            (CPFieldRef    i1 i2 ) -> OutputValues [ UI8 cpFieldRefTag       , UI16 (deIdx i1), UI16 (deIdx i2) ];
            (CPString      i     ) -> OutputValues [ UI8 cpStringInfoTag     , UI16 (deIdx i) ];
            (CPInteger     si32  ) -> OutputValues [ UI8 cpIntegerInfoTag    , SI32 si32 ];
            (CPFloat       float ) -> OutputValues [ UI8 cpFloatInfoTag      , UI32 $ floatToIEEE754 float ];
            (CPLong        si64  ) -> OutputValues [ UI8 cpLongInfoTag       , SI64 si64 ];
            (CPDouble      double) -> OutputValues [ UI8 cpDoubleInfoTag     , UI64 $ doubleToIEEE754 double ];
            (CPNameAndType i1 i2 ) -> OutputValues [ UI8 cpNameAndTypeInfoTag, UI16 (deIdx i1), UI16 (deIdx i2) ];
            (CPUtf8        s     ) -> OutputValues [ UI8 cpUtf8InfoTag       , OutputString $ let utf = convertToUTF8 s in (writeUI16 $ i2i $ length utf) ++ utf ];
            
            (CPMethodRef   i1 i2 False) -> OutputValues [ UI8 cpMethodRefTag         , UI16 (deIdx i1), UI16 (deIdx i2) ];
            (CPMethodRef   i1 i2 True ) -> OutputValues [ UI8 cpInterfaceMethodRefTag, UI16 (deIdx i1), UI16 (deIdx i2) ];
            
         }, restOut], (cpoolSlotCount e) + slots );

    deIdx :: CPoolIndex -> Word16;
    deIdx (CPoolIndex i ) = i;

    --Entry type tags
    cpClassInfoTag          = 7  ::Word8;
    cpFieldRefTag           = 9  ::Word8;
    cpMethodRefTag          = 10 ::Word8;
    cpInterfaceMethodRefTag = 11 ::Word8;
    cpStringInfoTag         = 8  ::Word8;
    cpIntegerInfoTag        = 3  ::Word8;
    cpFloatInfoTag          = 4  ::Word8;
    cpLongInfoTag           = 5  ::Word8;
    cpDoubleInfoTag         = 6  ::Word8;
    cpNameAndTypeInfoTag    = 12 ::Word8;
    cpUtf8InfoTag           = 1  ::Word8;
}
