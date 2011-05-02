-- | Model of a Java Class File at the file-format level
module Java.VM.Class.File ( 
    module Java.VM.Class.File, 
    module Java.VM.Class.File.CodeAttribute,
    module Java.VM.Class.File.Flags

) where {

    import Java.VM.Class.File.ConstantPool;
    import Java.VM.Class.File.CodeAttribute;
    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.Flags;
    import Data.Word;
    import Data.Bits;

    -- | Java Class File structure  
    data ClassFile = ClassFile { magicNumber      :: Word32,
                                 majorVersion     :: Word16,
                                 minorVersion     :: Word16,
                                 constantPool     :: ConstantPool,
                                 classFlags       :: Word16,
                                 classIndex       :: CPoolIndex,
                                 superClassIndex  :: CPoolIndex,
                                 interfaceIndices :: [CPoolIndex],
                                 fieldInfos       :: [FieldInfo],
                                 methodInfos      :: [MethodInfo],
                                 classAttributes  :: [ClassAttribute] }
        deriving Eq;

    -- | JVM FieldInfo structure    
    data FieldInfo = FieldInfo { fieldFlags       :: Word16,
                                 fieldNameIndex   :: CPoolIndex,
                                 fieldTypeIndex   :: CPoolIndex,
                                 fieldAttributes  :: [FieldAttribute] }
        deriving (Show,Eq);
                
    -- | JVM MethodInfo structure    
    data MethodInfo = MethodInfo { methodFlags      :: Word16,
                                   methodNameIndex  :: CPoolIndex,
                                   methodDescIndex  :: CPoolIndex,
                                   methodAttributes :: [MethodAttribute] }
        deriving (Show,Eq);
                
    -- | Possible Class Attributes
    data ClassAttribute = ClassAttribute   Attribute
                        | EnclosingMethod  CPoolIndex CPoolIndex -- ^ Class index, method index
                        | InnerClasses     [InnerClass]
                        | SourceFile       CPoolIndex
                        | ClassSignature   CPoolIndex
                        | SyntheticClass
                        | DeprecatedClass
                        | ClassAnnotations Bool [Annotation] -- ^Bool is True if runtime-visible
    
        deriving (Show,Eq);                
        
    -- | Possible Field Attributes
    data FieldAttribute = FieldAttribute   Attribute
                        | ConstantValue    CPoolIndex  -- ^ index of constant value
                        | FieldSignature   CPoolIndex
                        | SyntheticField
                        | DeprecatedField
                        | FieldAnnotations Bool [Annotation] -- ^Bool is True if runtime-visible
    
        deriving (Show,Eq);                
                
    -- | Possible Method Attributes
    data MethodAttribute = MethodAttribute   Attribute
                         | Code              CodeAttribute
                         | Exceptions        [CPoolIndex] -- ^ indices of thrown exceptions
                         | MethodSignature   CPoolIndex
                         | AnnotationDefault ElementValue
                         | SyntheticMethod
                         | DeprecatedMethod
                         | MethodAnnotations    Bool [Annotation]   -- ^Bool is True if runtime-visible
                         | ParameterAnnotations Bool [[Annotation]] -- ^Bool is True if runtime-visible
    
        deriving (Show,Eq);                        


    -- | Inner class structure
    data InnerClass = InnerClass {
                          inner_class_info_index   :: CPoolIndex,
                          outer_class_info_index   :: CPoolIndex,
                          inner_name_index         :: CPoolIndex,
                          inner_class_access_flags :: Word16
                      }
        deriving (Show,Eq);                        

    -- | An annotation
    data Annotation = Annotation CPoolIndex [(CPoolIndex,ElementValue)]  -- ^ type index, [(name,value)]
        deriving (Show,Eq);                        

    -- | An annotation element value
    data ElementValue = ElemValByte   CPoolIndex
                      | ElemValBool   CPoolIndex
                      | ElemValShort  CPoolIndex
                      | ElemValChar   CPoolIndex
                      | ElemValInt    CPoolIndex
                      | ElemValLong   CPoolIndex
                      | ElemValFloat  CPoolIndex
                      | ElemValDouble CPoolIndex
                      | ElemValString CPoolIndex
                      | ElemValEnum   CPoolIndex CPoolIndex -- ^ Type name, Const name
                      | ElemValClass  CPoolIndex
                      | ElemValAnnot  Annotation
                      | ElemValArray  [ElementValue]
        deriving (Show,Eq);                        

                 
	instance Show ClassFile where {
		show cf = "\nClassFile\n  majorVersion = " ++  (show $ majorVersion cf)	++
		          "\n  minorVersion = " ++ (show $ minorVersion cf) ++
		          "\n" ++ (show $ constantPool cf) ++
		          "\n  class flags = " ++ (show $ classFlags cf ) ++
		          "\n  name index  = " ++ (show $ classIndex cf ) ++
		          "\n  super index = " ++ (show $ superClassIndex cf ) ++
				  "\nInterfaces" ++
				  (concatMap (\fi->"\n  " ++ (show fi)) $ fieldInfos cf) ++
				  "\nFields" ++
				  "\nMethods" ++
				  "\nAttributes" ++
				  (concatMap (\ca->"\n  " ++ (show ca)) $ classAttributes cf);
	};    
}