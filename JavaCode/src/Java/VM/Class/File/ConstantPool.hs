-- | Constant Pool ADT and accessor functions
module Java.VM.Class.File.ConstantPool (

    ConstantPool,
    CPoolIndex,
    cpoolSize,
    cpoolEmpty,
    cpoolNullIndex,
    cpoolIsNullIndex,
    FieldRef,
    MethodRef,
    NameAndType,
    ConstantEntry(..),
    
    module Java.VM.Class.File.ConstantPool.Accessors,
    module Java.VM.Class.File.ConstantPool.Adders
    
) where {

    import Java.VM.Class.File.ConstantPool.Accessors;
    import Java.VM.Class.File.ConstantPool.Adders;
    import Java.VM.Class.File.ConstantPool.Model;

} 