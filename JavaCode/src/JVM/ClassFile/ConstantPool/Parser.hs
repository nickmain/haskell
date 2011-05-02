-- | Constant Pool parser
module JVM.ClassFile.ConstantPool.Parser (

    cpoolParse,
    parseCPoolIndex,
    parseCPoolIndex8
    
) where {

    import JVM.ClassFile.IO.ParserUtil;
    import JVM.ClassFile.ConstantPool;
    import Data.Word;
    import JVM.ClassFile.Util.JVMOp;
    import Control.Monad.Error;

    -- | Parse a Constant Pool
    cpoolParse :: (SourceContainer a, CPoolContainer a) => JVMOp a ();
    cpoolParse = do {
                     cpCount <- readUI16;
                     entries <- parseEntries 1 cpCount;
                     poolFromList entries
                 };
    
    poolFromList :: (CPoolContainer a) => [CPEntry] -> JVMOp a ();
    poolFromList entries = mapM_ cpoolAppendEntry entries;
    
    parseEntries :: (SourceContainer a) =>  Word16 {-index-} -> Word16 {-cp size-} -> JVMOp a [CPEntry];
    parseEntries index size = do {
                                  entry <- parseCPEntry;
                                  let newIndex = index + (cpoolSlotCount entry)
                                   in if newIndex >= size then return [entry] --last entry
                                      else do {
                                               entries <- parseEntries newIndex size;
                                               return (entry:entries); 
                                           };
                              };

    -- | Parse a 16 bit cpool index
    parseCPoolIndex :: (SourceContainer a) => JVMOp a CPoolIndex;
    parseCPoolIndex = do {
                          idx <- readUI16;
                          return idx;
                      };

    -- | Parse an 8 bit cpool index
    parseCPoolIndex8 :: (SourceContainer a) => JVMOp a CPoolIndex;
    parseCPoolIndex8 = do {
                          idx <- readUI8;
                          return $ fromIntegral idx;
                      };
    
    -- Read the entry-type tag and parse the appropriate entry type
    parseCPEntry :: (SourceContainer a) => JVMOp a CPEntry;
    parseCPEntry = do {
                       tag <- readUI8;
                       case tag of {
                           7  -> parseClassInfo;
                           9  -> parseFieldRef;
                           10 -> parseMethodRef;
                           11 -> parseIMethodRef;
                           8  -> parseStringInfo;
                           3  -> parseIntegerInfo;
                           4  -> parseFloatInfo;
                           5  -> parseLongInfo;
                           6  -> parseDoubleInfo;
                           12 -> parseNameTypeInfo;
                           1  -> parseUTF8Entry;
                           otherwise -> throwError ("unknown CP tag " ++ (show tag))
                       }
                   };
    
    parseUTF8Entry :: (SourceContainer a) => JVMOp a CPEntry;
    parseUTF8Entry = do {
                         length   <- readUI16; 
                         utfChars <- readSourceElements length;
                         return (CPUtf8 (decodeUTF8 utfChars))
                     };
    
    parseClassInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseClassInfo = do { 
        index <- readUI16; 
        return $ CPClass index 
    };
    
    parseFieldRef :: (SourceContainer a) => JVMOp a CPEntry;
    parseFieldRef = do { 
        classIndex <- readUI16; 
        nameTypeIndex <- readUI16; 
        return $ CPFieldRef classIndex nameTypeIndex
    };
    
    parseMethodRef :: (SourceContainer a) => JVMOp a CPEntry;
    parseMethodRef = do { 
        classIndex <- readUI16; 
        nameTypeIndex <- readUI16; 
        return $ CPMethodRef classIndex nameTypeIndex False
    };

    parseIMethodRef :: (SourceContainer a) => JVMOp a CPEntry;
    parseIMethodRef = do { 
        classIndex    <- readUI16; 
        nameTypeIndex <- readUI16; 
        return $ CPMethodRef classIndex nameTypeIndex True
    };

    parseStringInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseStringInfo = do { 
        index <- readUI16; 
        return $ CPString index 
    };

    parseIntegerInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseIntegerInfo = do { 
        value <- readSI32; 
        return $ CPInteger value 
    };  
    
    parseLongInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseLongInfo = do { 
        value <- readSI64; 
        return $ CPLong value 
    };    

    parseNameTypeInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseNameTypeInfo = do { 
        nameIndex <- readUI16; 
        typeIndex <- readUI16; 
        return $ CPNameAndType nameIndex typeIndex
    };

    parseDoubleInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseDoubleInfo = do { 
        value <- readDouble; 
        return $ CPDouble value 
    };

    parseFloatInfo :: (SourceContainer a) => JVMOp a CPEntry;
    parseFloatInfo = do { 
        value <- readFloat; 
        return $ CPFloat value 
    };
    
}
