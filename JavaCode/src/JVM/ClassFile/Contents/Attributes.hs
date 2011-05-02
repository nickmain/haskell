-- | Class file attribute data types
module JVM.ClassFile.Contents.Attributes where {

    import JVM.ClassFile.Contents.Types;
    import JVM.ClassFile.Contents.CodeAttribute;
    import JVM.Types;
    import Data.Word;
    
    -- | Possible Class Attributes
    data ClassAttribute = ClassAttribute   Attribute
                        | EnclosingMethod  MethodRef
                        | InnerClasses     [InnerClass]
                        | SourceFile       String
                        | ClassSignature   String
                        | SyntheticClass
                        | DeprecatedClass
                        | ClassAnnotations Bool [Annotation] -- ^Bool is True if runtime-visible
    
        deriving Show;                
        
    -- | Possible Field Attributes
    data FieldAttribute = FieldAttribute   Attribute
                        | ConstantField    ConstantValue  -- ^ index of constant value
                        | FieldSignature   JavaType
                        | SyntheticField
                        | DeprecatedField
                        | FieldAnnotations Bool [Annotation] -- ^Bool is True if runtime-visible
    
        deriving Show;                

    -- | Possible Method Attributes
    data MethodAttribute = MethodAttribute   Attribute
                         | Code              CodeAttribute
                         | Exceptions        [JavaType] 
                         | MethodSignature   MethodSig
                         | AnnotationDefault ElementValue
                         | SyntheticMethod
                         | DeprecatedMethod
                         | MethodAnnotations    Bool [Annotation]   -- ^Bool is True if runtime-visible
                         | ParameterAnnotations Bool [[Annotation]] -- ^Bool is True if runtime-visible
    
        deriving Show;                        

    -- | Inner class structure
    data InnerClass = InnerClass {
                          innerClassName  :: JavaClassName,
                          outerClassName  :: JavaClassName,
                          innerName       :: JavaClassName,
                          innerClassFlags :: Word16
                      }
        deriving Show;                        

    
}
