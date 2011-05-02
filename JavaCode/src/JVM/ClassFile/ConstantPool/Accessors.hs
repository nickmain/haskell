-- | Functions for accessing constant pool entries.
module JVM.ClassFile.ConstantPool.Accessors (

    cpoolGetClass,
    cpoolGetNameAndType,
    cpoolGetFieldRef,
    cpoolGetMethodRef,
    cpoolGetString,
    cpoolGetInt,
    cpoolGetFloat,
    cpoolGetLong,
    cpoolGetDouble,
    cpoolGetUTF8,
    cpoolGetConstant,
    FieldRef,
    MethodRef,
    NameAndType,
    ConstantValue(..)

) where {

    import JVM.ClassFile.ConstantPool.Model;
    import JVM.ClassFile.Contents.Types;
    import JVM.Types.Utils;
    import JVM.ClassFile.Util.JVMOp;
    import Data.Int;


    -- | A name and type: (name,type-string).
    type NameAndType = (String,String); 

    -- | Get the class ref at the given index.
    cpoolGetClass :: (CPoolContainer a) => CPoolIndex -> JVMOp a JavaClassName;
    cpoolGetClass i = do {
        (CPClass idx) <- cpoolEntryAt i;
        s             <- cpoolGetUTF8 idx;
        return (jvmFormatToClassName s);
    };

    -- | Get the name-and-type at the given index
    cpoolGetNameAndType :: (CPoolContainer a) => CPoolIndex -> JVMOp a NameAndType;
    cpoolGetNameAndType i = do {
        (CPNameAndType ni ti) <- cpoolEntryAt i;
        name                  <- cpoolGetUTF8 ni;
        typ                   <- cpoolGetUTF8 ti;
        return (name,typ);
    };

    -- | Get the field ref at the given index
    cpoolGetFieldRef :: (CPoolContainer a) => CPoolIndex -> JVMOp a FieldRef;
    cpoolGetFieldRef i = do {
        (CPFieldRef ci nti) <- cpoolEntryAt i;
        className           <- cpoolGetClass ci;
        (name,typ)          <- cpoolGetNameAndType nti;
        return $ FieldRef className name (jtypeFromSig typ);
    };                        

    -- | Get the method ref at the given index
    cpoolGetMethodRef :: (CPoolContainer a) => CPoolIndex -> JVMOp a MethodRef;
    cpoolGetMethodRef i = do {
        (CPMethodRef ci nti _) <- cpoolEntryAt i;
        className              <- cpoolGetClass ci;
        (name,sig)             <- cpoolGetNameAndType nti;
        return $ MethodRef className name (signatureFromJVMSig sig);
    };                        
                            
    -- | Get the string value at the given index.
    cpoolGetString :: (CPoolContainer a) => CPoolIndex -> JVMOp a String;
    cpoolGetString i = do {
        (CPString idx) <- cpoolEntryAt i;
        s              <- cpoolGetUTF8 idx;
        return s
    };

    -- | Get the integer value at the given index.
    cpoolGetInt :: (CPoolContainer a) => CPoolIndex -> JVMOp a Int32;
    cpoolGetInt i = do {
        (CPInteger v) <- cpoolEntryAt i;
        return v
    };

    -- | Get the float value at the given index.
    cpoolGetFloat :: (CPoolContainer a) => CPoolIndex -> JVMOp a Float;
    cpoolGetFloat i = do {
        (CPFloat v) <- cpoolEntryAt i;
        return v
    };

    -- | Get the long value at the given index.
    cpoolGetLong :: (CPoolContainer a) => CPoolIndex -> JVMOp a Int64;
    cpoolGetLong i = do {
        (CPLong v) <- cpoolEntryAt i;
        return v
    };

    -- | Get the double value at the given index.
    cpoolGetDouble ::  (CPoolContainer a) => CPoolIndex -> JVMOp a Double;
    cpoolGetDouble i = do {
        (CPDouble v) <- cpoolEntryAt i;
        return v
    };
                            
    -- | Get the utf8 value at the given index.
    cpoolGetUTF8 :: (CPoolContainer a) => CPoolIndex -> JVMOp a String;
    cpoolGetUTF8 i = do {
        (CPUtf8 s) <- cpoolEntryAt i;
        return s
    };

    -- | Get a constant value
    cpoolGetConstant :: (CPoolContainer a) => CPoolIndex -> JVMOp a ConstantValue;
    cpoolGetConstant i = do {
        entry <- cpoolEntryAt i;
        case entry of {
            (CPString   idx) -> do { 
                                    s <- cpoolGetUTF8 idx;
                                    return ( ConstString s );
                                };
            (CPClass    idx) -> do { 
                                    name <- cpoolGetUTF8 idx;
                                    return ( ConstClass $ jtypeFromSig name );
                                };
            (CPInteger  val) -> return ( ConstInt    val );
            (CPFloat    val) -> return ( ConstFloat  val );
            (CPLong     val) -> return ( ConstLong   val );
            (CPDouble   val) -> return ( ConstDouble val );

            otherwise -> fail $ "Invalid constant: " ++ (show entry);
        }
    };
}
