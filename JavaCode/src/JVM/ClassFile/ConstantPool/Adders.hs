-- | Functions for adding constant pool entries.
module JVM.ClassFile.ConstantPool.Adders (

    cpoolAddUTF8,
    cpoolAddClass,
    cpoolAddString,
    cpoolAddInt,
    cpoolAddFloat,
    cpoolAddLong,
    cpoolAddDouble,
    cpoolAddFieldRef,
    cpoolAddMethodRef,
    cpoolAddInterfaceMethodRef,
    cpoolAddNameAndType  
     
) where {

    import JVM.ClassFile.ConstantPool.Model;
    import JVM.ClassFile.Util.JVMOp;
    import JVM.Types.Utils;
    import Data.Int;
        
    -- | Add a utf8 string to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddUTF8 :: (CPoolContainer a) => String -> JVMOp a CPoolIndex;
    cpoolAddUTF8 s = cpoolAddEntry (CPUtf8 s);
    
    -- | Add a class to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddClass :: (CPoolContainer a) => JavaClassName -> JVMOp a CPoolIndex;
    cpoolAddClass name = do {
        utf <- cpoolAddUTF8 (classNameToJVMFormat name);
        cpoolAddEntry (CPClass utf) 
    };

    -- | Add a string constant to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddString :: (CPoolContainer a) => String -> JVMOp a CPoolIndex;
    cpoolAddString s = do {
        utf <- cpoolAddUTF8 s;
        cpoolAddEntry (CPString utf)
    };

    -- | Add an integer constant to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddInt :: (CPoolContainer a) => Int32 -> JVMOp a CPoolIndex;
    cpoolAddInt val = cpoolAddEntry (CPInteger val);
    
    -- | Add a float constant to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddFloat :: (CPoolContainer a) => Float -> JVMOp a CPoolIndex;
    cpoolAddFloat val = cpoolAddEntry (CPFloat val);

    -- | Add a long constant to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddLong :: (CPoolContainer a) => Int64 -> JVMOp a CPoolIndex;
    cpoolAddLong val = cpoolAddEntry (CPLong val);

    -- | Add a double constant to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddDouble :: (CPoolContainer a) => Double -> JVMOp a CPoolIndex;
    cpoolAddDouble val = cpoolAddEntry (CPDouble val);
                  
    -- | Add a field ref to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddFieldRef :: (CPoolContainer a) => 
                        JavaClassName -- ^ class name
                     -> String        -- ^ field name
                     -> JavaType      -- ^ field type
                     -> JVMOp a CPoolIndex;
    cpoolAddFieldRef cname fname typ = do {
        classIdx <- cpoolAddClass cname;
        ntIdx    <- cpoolAddNameAndType fname (jtypeToSig typ);
        cpoolAddEntry (CPFieldRef classIdx ntIdx)
    };
                  
    -- | Add a method ref to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddMethodRef :: (CPoolContainer a) => 
                         JavaClassName -- ^ class name
                      -> String        -- ^ method name
                      -> MethodSig     -- ^ method signature
                      -> JVMOp a CPoolIndex;
    cpoolAddMethodRef cname mname sig = addMethodRef cname mname sig False;
    
    -- | Add an iterface method ref to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddInterfaceMethodRef :: (CPoolContainer a) => 
                                  JavaClassName -- ^ class name
                               -> String        -- ^ method name
                               -> MethodSig     -- ^ method signature
                               -> JVMOp a CPoolIndex;
    cpoolAddInterfaceMethodRef cname mname sig = addMethodRef cname mname sig True;        

    -- | Add a name-and-type to the pool, or access the existing entry, 
    --   returns the index.
    cpoolAddNameAndType :: (CPoolContainer a) => 
                           String  -- ^ name
                        -> String  -- ^ type string
                        -> JVMOp a CPoolIndex;
    cpoolAddNameAndType name typ = do {
        nameUtf <- cpoolAddUTF8 name;
        typeUtf <- cpoolAddUTF8 typ;
        cpoolAddEntry (CPNameAndType nameUtf typeUtf)
    };
        
    -- Add a method ref to the pool, or access the existing entry, 
    -- returns the index.
    addMethodRef :: (CPoolContainer a) => 
                    JavaClassName -- ^ class name
                 -> String        -- ^ method name
                 -> MethodSig     -- ^ method signature
                 -> Bool          -- ^ True for an interface method
                 -> JVMOp a CPoolIndex;
    addMethodRef cname mname sig iface = do {
        classIdx <- cpoolAddClass cname;
        ntIdx    <- cpoolAddNameAndType mname (signatureToJVMSig sig);
        cpoolAddEntry (CPMethodRef classIdx ntIdx iface)
    };

}
