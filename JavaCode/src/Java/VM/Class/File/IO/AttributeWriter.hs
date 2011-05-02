-- | Attribute writing functions
module Java.VM.Class.File.IO.AttributeWriter where {

    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File;
    import Java.VM.Class.File.IO.WriterUtil;
    import Java.VM.Class.File.ConstantPool;
    import Java.VM.Class.File.ConstantPool.Writer;


    -- | Write a JVM Classfile attribute
    writeAttribute :: Attribute -> OutputValue;    
    writeAttribute (Attribute name size bytes) = OutputValues [ writeCPoolIndex name, UI32 size, OutputString $ bytesToString bytes ];


	-- | Write an array of annotations, prefixed by the length
	writeAnnots :: [Annotation] -> OutputValue;
	writeAnnots aa = writeItems aa writeAnnot;
	
	-- | Write an annotation
	writeAnnot :: Annotation -> OutputValue;
	writeAnnot a = OutputNothing;
}
