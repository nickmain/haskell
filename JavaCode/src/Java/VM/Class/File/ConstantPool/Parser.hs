-- | Constant Pool parser
module Java.VM.Class.File.ConstantPool.Parser (

    cpoolParse,
    parseCPoolIndex,
    parseCPoolIndex8
    
) where {

    import Java.VM.Class.File.IO.ParserUtil;
    import Java.VM.Class.File.ConstantPool.Model;
    import Data.Word;

    -- | Parse a Constant Pool
    cpoolParse :: ParseComputation ConstantPool;
    cpoolParse = do {
                     cpCount <- readUI16;
                     entries <- parseEntries 1 cpCount;
                     return $ poolFromList entries;
                 };
    
    poolFromList :: [CPEntry] -> ConstantPool;
    poolFromList entries = foldl (\cp e -> snd (cpoolAppendEntry cp e)) cpoolEmpty entries;
    
    parseEntries :: Word16 {-index-} -> Word16 {-cp size-} -> ParseComputation [CPEntry];
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
    parseCPoolIndex :: ParseComputation CPoolIndex;
    parseCPoolIndex = do {
                          idx <- readUI16;
                          return $ CPoolIndex idx;
                      };

    -- | Parse an 8 bit cpool index
    parseCPoolIndex8 :: ParseComputation CPoolIndex;
    parseCPoolIndex8 = do {
                          idx <- readUI8;
                          return $ CPoolIndex $ fromIntegral idx;
                      };
    
    parseCPEntry :: ParseComputation CPEntry;
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
                           otherwise -> fail ("unknown CP tag " ++ (show tag));
                       };
                   };
    
    parseUTF8Entry :: ParseComputation CPEntry;
    parseUTF8Entry = do {
                         length   <- readUI16; 
                         utfChars <- readSourceElements length;
                         return (CPUtf8 (decodeUTF8 utfChars));
                     };
    
    parseClassInfo :: ParseComputation CPEntry;
    parseClassInfo = do { index <- readUI16; return $ CPClass $ CPoolIndex index };
    
    parseFieldRef :: ParseComputation CPEntry;
    parseFieldRef = do { classIndex <- readUI16; nameTypeIndex <- readUI16; return $ CPFieldRef (CPoolIndex classIndex) (CPoolIndex nameTypeIndex) };
    
    parseMethodRef :: ParseComputation CPEntry;
    parseMethodRef = do { classIndex <- readUI16; nameTypeIndex <- readUI16; return $ CPMethodRef (CPoolIndex classIndex) (CPoolIndex nameTypeIndex) False};

    parseIMethodRef :: ParseComputation CPEntry;
    parseIMethodRef = do { classIndex <- readUI16; nameTypeIndex <- readUI16; return $ CPMethodRef (CPoolIndex classIndex) (CPoolIndex nameTypeIndex) True};

    parseStringInfo :: ParseComputation CPEntry;
    parseStringInfo = do { index <- readUI16; return $ CPString $ CPoolIndex index };

    parseIntegerInfo :: ParseComputation CPEntry;
    parseIntegerInfo = do { value <- readSI32; return $ CPInteger value };  
    
    parseLongInfo :: ParseComputation CPEntry;
    parseLongInfo = do { value <- readSI64; return $ CPLong value };    

    parseNameTypeInfo :: ParseComputation CPEntry;
    parseNameTypeInfo = do { nameIndex <- readUI16; typeIndex <- readUI16; return $ CPNameAndType (CPoolIndex nameIndex) (CPoolIndex typeIndex) };

    parseDoubleInfo :: ParseComputation CPEntry;
    parseDoubleInfo = do { value <- readDouble; return $ CPDouble value };

    parseFloatInfo :: ParseComputation CPEntry;
    parseFloatInfo = do { value <- readFloat; return $ CPFloat value };
    

}
