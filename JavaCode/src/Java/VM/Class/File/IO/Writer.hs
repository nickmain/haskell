-- | Writer for the JVM Class File format
module Java.VM.Class.File.IO.Writer ( 
    writeClassFile, 
    writeClassToFile, 
    writeItems, 
    writeAttribute 
) where {

    import Java.VM.Class.File;
    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.IO.WriterUtil;
    import Java.VM.Class.File.IO.ParserUtil hiding (i2i);    
    import Java.VM.Class.File.ConstantPool;
    import Java.VM.Class.File.ConstantPool.Writer;
    import Java.VM.Class.File.CodeAttribute.Writer;
    import Java.VM.Class.File.IO.AttributeWriter;
    import Java.VM.Class.File.IO.AttributeNames;
    import System.IO;
    
    -- | Convert a classfile to JVM spec bytes and write to disk
    writeClassToFile :: ClassFile -> FilePath -> IO ();
    writeClassToFile cf filename = do {
                                       handle <- openBinaryFile filename WriteMode;
                                       hPutStr handle $ writeOutputValue $ writeClassFile cf;
                                       hClose handle;
                                   };
    
    -- | Convert a classfile to JVM spec bytes
    writeClassFile :: ClassFile -> OutputValue;
    writeClassFile cf = 
        let cp = constantPool cf
            (methodsOut,cp2) = convertList writeMethod (methodInfos cf) cp 
            (fieldsOut,cp3)  = convertList writeField  (fieldInfos  cf) cp2
            (rawAttrs,newCP) = convertList classAttributeToRawData (classAttributes cf) cp3
         in OutputValues [        
                UI32 $ magicNumber  cf,
                UI16 $ minorVersion cf,
                UI16 $ majorVersion cf,
                cpoolWrite newCP,
                UI16 $ classFlags  cf,
                writeCPoolIndex $ classIndex       cf,
                writeCPoolIndex $ superClassIndex  cf,
                writeItems (interfaceIndices cf) writeCPoolIndex,
                UI16 $ i2i (length fieldsOut),
                OutputValues fieldsOut,
                UI16 $ i2i (length methodsOut),
                OutputValues methodsOut,
                UI16 $ i2i (length rawAttrs),                
                writeItems rawAttrs writeAttribute];
                                                          
    
    writeField :: FieldInfo -> ConstantPool -> (OutputValue,ConstantPool);
    writeField (FieldInfo flags nameIdx typeIdx attrs) cp =
        let (rawAttrs,newCP) = convertList fieldAttributeToRawData attrs cp    
         in (OutputValues [
                UI16 flags,
                writeCPoolIndex nameIdx,
                writeCPoolIndex typeIdx,
                writeItems rawAttrs writeAttribute ], cp);
     
    writeMethod :: MethodInfo -> ConstantPool -> (OutputValue,ConstantPool);
    writeMethod (MethodInfo flags nameIdx descIdx attrs) cp = 
        let (rawAttrs,newCP) = convertList methodAttributeToRawData attrs cp
         in (OutputValues [
                UI16 flags,
                writeCPoolIndex nameIdx,
                writeCPoolIndex descIdx,
                writeItems rawAttrs writeAttribute], newCP);
                                          

    -- convert attributes to raw data
    classAttributeToRawData :: ClassAttribute -> ConstantPool -> (Attribute,ConstantPool);
    classAttributeToRawData ca cp = case ca of {
        (ClassAttribute  a    ) -> (a,cp);
		(EnclosingMethod i1 i2) -> makeAttribute attrName_EnclosingMethod (OutputValues [writeCPoolIndex i1, writeCPoolIndex i2]) cp;
        (SourceFile       i   ) -> makeAttribute attrName_SourceFile      (writeCPoolIndex i) cp;
        (ClassSignature   i   ) -> makeAttribute attrName_Signature       (writeCPoolIndex i) cp;
        SyntheticClass          -> makeAttribute attrName_Synthetic       OutputNothing cp;
        DeprecatedClass         -> makeAttribute attrName_Deprecated      OutputNothing cp;
        (InnerClasses    icc  ) -> makeAttribute attrName_InnerClasses    (OutputValues $ map writeInnerClass icc) cp;
     --   (ClassAnnotations v aa) -> 
    };                                               
    
    makeAttribute :: String {- name -} -> OutputValue -> ConstantPool -> (Attribute,ConstantPool);
    makeAttribute n v cp = let (ni,cp2) = cpoolAddUTF8 cp n
                            in (Attribute ni (i2i $sizeOfOutputValue v) $ stringToSource $ writeOutputValue v,cp2);
    
    
    writeInnerClass :: InnerClass -> OutputValue;
    writeInnerClass (InnerClass i1 i2 i3 flgs) = OutputValues [
    	writeCPoolIndex i1,
    	writeCPoolIndex i2,
    	writeCPoolIndex i3,
    	UI16 flgs
    ];
    
    -- convert attributes to raw data
    fieldAttributeToRawData :: FieldAttribute -> ConstantPool -> (Attribute,ConstantPool) ;
    fieldAttributeToRawData ca cp = case ca of {
        (FieldAttribute a) -> (a,cp);
        otherwise -> (Attribute cpoolNullIndex 0 [],cp);
    };                                               
    
    -- convert attributes to raw data
    methodAttributeToRawData :: MethodAttribute -> ConstantPool -> (Attribute,ConstantPool);
    methodAttributeToRawData ca cp = case ca of {
        (MethodAttribute a) -> (a,cp);
        (Code            c) -> let (idx,cp2) = cpoolAddUTF8 cp attrName_Code
                                in (writeCodeAttribute c idx, cp2);
        otherwise -> (Attribute cpoolNullIndex 0 [], cp);
    };                                               
    
    
    convertList :: (a -> b -> (c,b)) -> [a] -> b -> ([c],b);
    convertList _ [] b     = ([],b);
    convertList f (a:aa) b = let (c ,b2) = f a b
                                 (cc,bz) = convertList f aa b2
                              in (c:cc,bz);
    
    
    
    ---------------------------------------------------------------------------
    -- Attribute types..
    ---------------------------------------------------------------------------
}