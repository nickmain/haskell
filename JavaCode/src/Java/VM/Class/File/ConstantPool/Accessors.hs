-- | Functions for accessing constant pool entries.
module Java.VM.Class.File.ConstantPool.Accessors (

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
    cpoolGetConstant

) where {

    import Java.VM.Class.File.ConstantPool.Model;
    import Java.Types.Utils;
    import Data.Int;

   -- | Get the class ref at the given index.
    cpoolGetClass :: (Monad m) => ConstantPool -> CPoolIndex -> m JavaClassName;
    cpoolGetClass cp i = do {
        (CPClass idx) <- cpoolEntryAt cp i;
        s             <- cpoolGetUTF8 cp idx;
        return (jvmFormatToClassName s);
    };

    -- | Get the name-and-type at the given index
    cpoolGetNameAndType :: (Monad m) => ConstantPool -> CPoolIndex -> m NameAndType;
    cpoolGetNameAndType cp i = do {
        (CPNameAndType ni ti) <- cpoolEntryAt cp i;
        name                  <- cpoolGetUTF8 cp ni;
        typ                   <- cpoolGetUTF8 cp ti;
        return (name,typ);
    };

    -- | Get the field ref at the given index
    cpoolGetFieldRef :: (Monad m) => ConstantPool -> CPoolIndex -> m FieldRef;
    cpoolGetFieldRef cp i = do {
        (CPFieldRef ci nti) <- cpoolEntryAt cp i;
        className           <- cpoolGetClass cp ci;
        (name,typ)          <- cpoolGetNameAndType cp nti;
        return (className, name, jtypeFromSig typ);
    };                        

    -- | Get the method ref at the given index
    cpoolGetMethodRef :: (Monad m) => ConstantPool -> CPoolIndex -> m MethodRef;
    cpoolGetMethodRef cp i = do {
        (CPMethodRef ci nti _) <- cpoolEntryAt cp i;
        className              <- cpoolGetClass cp ci;
        (name,sig)             <- cpoolGetNameAndType cp nti;
        return (className, name, signatureFromJVMSig sig);
    };                        
                            
    -- | Get the string value at the given index.
    cpoolGetString :: (Monad m) => ConstantPool -> CPoolIndex -> m String;
    cpoolGetString cp i = do {
        (CPString idx) <- cpoolEntryAt cp i;
        s              <- cpoolGetUTF8 cp idx;
        return s
    };

    -- | Get the integer value at the given index.
    cpoolGetInt :: (Monad m) => ConstantPool -> CPoolIndex -> m Int32;
    cpoolGetInt cp i = do {
        (CPInteger v) <- cpoolEntryAt cp i;
        return v
    };

    -- | Get the float value at the given index.
    cpoolGetFloat :: (Monad m) => ConstantPool -> CPoolIndex -> m Float;
    cpoolGetFloat cp i = do {
        (CPFloat v) <- cpoolEntryAt cp i;
        return v
    };

    -- | Get the long value at the given index.
    cpoolGetLong :: (Monad m) => ConstantPool -> CPoolIndex -> m Int64;
    cpoolGetLong cp i = do {
        (CPLong v) <- cpoolEntryAt cp i;
        return v
    };

    -- | Get the double value at the given index.
    cpoolGetDouble :: (Monad m) => ConstantPool -> CPoolIndex -> m Double;
    cpoolGetDouble cp i = do {
        (CPDouble v) <- cpoolEntryAt cp i;
        return v
    };
                            
    -- | Get the utf8 value at the given index.
    cpoolGetUTF8 :: (Monad m) => ConstantPool -> CPoolIndex -> m String;
    cpoolGetUTF8 cp i = do {
        (CPUtf8 s) <- cpoolEntryAt cp i;
        return s
    };

    -- | Get a constant value
    cpoolGetConstant :: (Monad m) => ConstantPool -> CPoolIndex -> m ConstantEntry;
    cpoolGetConstant cp i = do {
        entry <- cpoolEntryAt cp i;
        case entry of {
            (CPString   idx) -> do { 
                                    s <- cpoolGetUTF8 cp idx;
                                    return ( ConstString s );
                                };
            (CPClass    idx) -> do { 
                                    name <- cpoolGetUTF8 cp idx;
                                    return ( ConstClass name );
                                };
            (CPInteger  val) -> return ( ConstInt    val );
            (CPFloat    val) -> return ( ConstFloat  val );
            (CPLong     val) -> return ( ConstLong   val );
            (CPDouble   val) -> return ( ConstDouble val );

            otherwise -> fail "Invalid constant.";
        }
    };
}
