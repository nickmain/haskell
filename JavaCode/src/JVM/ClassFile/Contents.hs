-- | Datatypes for the contents of a JVM Class file
module JVM.ClassFile.Contents (

    module Data.Word,
    module Data.Int,   
    module JVM.Types,
    module JVM.ClassFile.Contents,
    module JVM.ClassFile.Contents.Types,
    module JVM.ClassFile.Contents.Attributes,
    module JVM.ClassFile.Contents.CodeAttribute

) where {

    import Data.Word;
    import Data.Int;    
    import JVM.Types;
    import JVM.ClassFile.Contents.Types;
    import JVM.ClassFile.Contents.Attributes;
    import JVM.ClassFile.Contents.CodeAttribute;

    data ClassFileVersion = ClassFileVersion { majorVersion, minorVersion :: Word16 };

    -- | Java Class File structure  
    data JVMClass = JVMClass { className        :: JavaClassName,
                               superclassName   :: JavaClassName,  -- empty string if none
                               interfaceNames   :: [JavaClassName],
                               fileVersion      :: ClassFileVersion,
                               classFlags       :: Word16,
                               classFields      :: [ClassField],
                               classMethods     :: [ClassMethod],
                               classAttributes  :: [ClassAttribute] };

    -- | JVM FieldInfo structure    
    data ClassField = ClassField { fieldName       :: String,
                                   fieldType       :: JavaType,
                                   fieldFlags      :: Word16,
                                   fieldAttributes :: [FieldAttribute] };
                
    -- | JVM MethodInfo structure    
    data ClassMethod = ClassMethod { methodName       :: String,
                                     methodSig        :: MethodSig,
                                     methodFlags      :: Word16,
                                     thrownExceptions :: [JavaType],
                                     methodAttributes :: [MethodAttribute] };

}
