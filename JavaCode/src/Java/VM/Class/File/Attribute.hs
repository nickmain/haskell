-- | The Attribute data type
module Java.VM.Class.File.Attribute where {

    import Data.Word;
    import Java.VM.Class.File.ConstantPool(CPoolIndex);

    data Attribute = Attribute { attrNameIndex :: CPoolIndex,
                                 attrSize      :: Word32,
                                 attrData      :: [Word8] }
        deriving (Show,Eq);
}