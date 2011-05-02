-- | Various data types used in a class file
module JVM.ClassFile.Contents.Types where {

    import JVM.Types;
    import Data.Int;
    import Data.Word;

    -- | The possible constant values
    data ConstantValue = ConstInt    Int32
                       | ConstFloat  Float
                       | ConstLong   Int64
                       | ConstDouble Double
                       | ConstString String
                       | ConstClass  JavaType
        deriving (Eq, Show);

    -- | Reference to a field                
    data FieldRef = FieldRef JavaClassName String JavaType
        deriving (Show,Eq);
                        
    -- | Reference to a method
    data MethodRef = MethodRef JavaClassName String MethodSig
        deriving (Show,Eq);
        
                
    -- | A raw attribute
    data Attribute = Attribute { attrName :: String,
                                 attrSize :: Word32,
                                 attrData :: [Word8] }
        deriving (Show,Eq);           
        
    -- | An annotation
    data Annotation = Annotation JavaType [(String,ElementValue)]  -- ^ type, [(name,value)]
        deriving (Show,Eq);                        

    -- | An annotation element value
    data ElementValue = ElemValByte   Int8
                      | ElemValBool   Bool
                      | ElemValShort  Int16
                      | ElemValChar   Char
                      | ElemValInt    Int32
                      | ElemValLong   Int64
                      | ElemValFloat  Float
                      | ElemValDouble Double
                      | ElemValString String
                      | ElemValEnum   JavaType String -- ^ Type name, Const name
                      | ElemValClass  JavaType
                      | ElemValAnnot  Annotation
                      | ElemValArray  [ElementValue]
        deriving (Show,Eq);                        
        
}
