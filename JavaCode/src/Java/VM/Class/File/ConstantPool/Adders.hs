-- | Functions for adding constant pool entries.
module Java.VM.Class.File.ConstantPool.Adders (

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

    import Java.VM.Class.File.ConstantPool.Model;
    import Java.Types.Utils;
    import Data.Int;
        
    -- | Add a utf8 string to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddUTF8 ::  ConstantPool -> String -> (CPoolIndex,ConstantPool);
    cpoolAddUTF8 cp s = cpoolAddEntry cp (CPUtf8 s);
    
    -- | Add a class to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddClass :: ConstantPool -> JavaClassName -> (CPoolIndex,ConstantPool);
    cpoolAddClass cp name = let (utf,cp2) = cpoolAddUTF8 cp (classNameToJVMFormat name)
                             in cpoolAddEntry cp2 (CPClass utf);

    -- | Add a string constant to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddString :: ConstantPool -> String -> (CPoolIndex,ConstantPool);
    cpoolAddString cp s = let (utf,cp2) = cpoolAddUTF8 cp s
                           in cpoolAddEntry cp2 (CPString utf);

    -- | Add an integer constant to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddInt ::  ConstantPool -> Int32 -> (CPoolIndex,ConstantPool);
    cpoolAddInt cp val = cpoolAddEntry cp (CPInteger val);
    
    -- | Add a float constant to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddFloat ::  ConstantPool -> Float -> (CPoolIndex,ConstantPool);
    cpoolAddFloat cp val = cpoolAddEntry cp (CPFloat val);

    -- | Add a long constant to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddLong ::  ConstantPool -> Int64 -> (CPoolIndex,ConstantPool);
    cpoolAddLong cp val = cpoolAddEntry cp (CPLong val);

    -- | Add a double constant to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddDouble ::  ConstantPool -> Double -> (CPoolIndex,ConstantPool);
    cpoolAddDouble cp val = cpoolAddEntry cp (CPDouble val);
                  
    -- | Add a field ref to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddFieldRef :: ConstantPool 
                     -> JavaClassName -- ^ class name
                     -> String        -- ^ field name
                     -> JavaType      -- ^ field type
                     -> (CPoolIndex,ConstantPool);
    cpoolAddFieldRef cp cname fname typ = let (classIdx,cp2) = cpoolAddClass cp cname
                                              (ntIdx   ,cp3) = cpoolAddNameAndType cp2 fname (jtypeToSig typ)
                                           in cpoolAddEntry cp3 (CPFieldRef classIdx ntIdx);
                  
    -- | Add a method ref to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddMethodRef :: ConstantPool 
                      -> JavaClassName -- ^ class name
                      -> String        -- ^ method name
                      -> Signature     -- ^ method signature
                      -> (CPoolIndex,ConstantPool);
    cpoolAddMethodRef cp cname mname sig = addMethodRef cp cname mname sig False;
    
    -- | Add an iterface method ref to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddInterfaceMethodRef :: ConstantPool 
                               -> JavaClassName -- ^ class name
                               -> String        -- ^ method name
                               -> Signature     -- ^ method signature
                               -> (CPoolIndex,ConstantPool);
    cpoolAddInterfaceMethodRef cp cname mname sig = addMethodRef cp cname mname sig True;        

    -- | Add a name-and-type to the pool, or access the existing entry, 
    --   returns the index and the updated pool.
    cpoolAddNameAndType :: ConstantPool 
                        -> String  -- ^ name
                        -> String  -- ^ type string
                        -> (CPoolIndex,ConstantPool);
    cpoolAddNameAndType cp name typ = let (nameUtf,cp2) = cpoolAddUTF8 cp name
                                          (typeUtf,cp3) = cpoolAddUTF8 cp2 typ
                                       in cpoolAddEntry cp3 (CPNameAndType nameUtf typeUtf);
        
    -- Add a method ref to the pool, or access the existing entry, 
    -- returns the index and the updated pool.
    addMethodRef :: ConstantPool 
                 -> JavaClassName -- ^ class name
                 -> String        -- ^ method name
                 -> Signature     -- ^ method signature
                 -> Bool          -- ^ True for an interface method
                 -> (CPoolIndex,ConstantPool);
    addMethodRef cp cname mname sig iface = let (classIdx,cp2) = cpoolAddClass cp cname
                                                (ntIdx   ,cp3) = cpoolAddNameAndType cp2 mname (signatureToJVMSig sig)
                                             in cpoolAddEntry cp3 (CPMethodRef classIdx ntIdx iface);    

}
