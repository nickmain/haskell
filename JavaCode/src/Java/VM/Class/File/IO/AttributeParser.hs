-- | Attribute parser functions
module Java.VM.Class.File.IO.AttributeParser where {

    import Java.VM.Class.File;
    import Java.VM.Class.File.ConstantPool.Parser;
    import Java.VM.Class.File.IO.ParserUtil;
    import Java.VM.Class.File.Attribute;
    import qualified Data.Map as Map;


    parseAttributes :: ParseComputation [Attribute];
    parseAttributes = parseItemsUI16 parseAttribute;
    
    parseAttribute :: ParseComputation Attribute;
    parseAttribute = do {
        nameIndex <- parseCPoolIndex;
        size      <- readUI32;
        attrData  <- readSourceElements (fromEnum size);
        return $ Attribute nameIndex size attrData;
    };
    
    ----------------------------------------------------------------------
    -- Common Attribute Parsers
    ----------------------------------------------------------------------

    parseIdxAttr :: (CPoolIndex -> a) -> ParseComputation a;
    parseIdxAttr c = do {
        idx <- parseCPoolIndex;
        return $ c idx;
    };

    parseAttr :: Map.Map String (ParseComputation a) -- parser map
              -> (Attribute -> a)                    -- constructor
              -> ConstantPool 
              -> Attribute                           -- attribute to parse
              -> a;
    parseAttr m c cp attr@(Attribute nameIdx _ bytes) = case do {
        name   <- cpoolGetUTF8 cp nameIdx;
        parser <- Map.lookup name m;
        return $ parse bytes parser 
    } of { 
        (Just (ParseSuccess a)) -> a;
        otherwise               -> c attr; 
    };
    
    ----------------------------------------------------------------------
    -- Annotation Parser
    ----------------------------------------------------------------------    
    
    parseAnnots :: (Bool -> [Annotation] -> a) -- constructor
                -> Bool -- whether visible
                -> ParseComputation a;
    parseAnnots c v = do {
        annotations <- parseItemsUI16 parseAnnotation;
        return $ c v annotations
    };          
                
    
    parseAnnotation :: ParseComputation Annotation;
    parseAnnotation = do {
        typeIndex <- parseCPoolIndex;                    
        elemVals  <- parseItemsUI16 parseNameAndValue;
        return $ Annotation typeIndex elemVals;
    };

    parseNameAndValue :: ParseComputation (CPoolIndex,ElementValue);
    parseNameAndValue = do {
        nameIdx <- parseCPoolIndex;
        elemVal <- parseElementValue;
        return (nameIdx,elemVal);
    };
    
    
    parseElementValue :: ParseComputation ElementValue;
    parseElementValue = do {
        tag <- readChar;
        case tag of {
            'B' -> parseElementConst ElemValByte;
            'C' -> parseElementConst ElemValChar;
            'S' -> parseElementConst ElemValShort;
            'Z' -> parseElementConst ElemValBool;
            'I' -> parseElementConst ElemValInt;
            'J' -> parseElementConst ElemValLong;
            'F' -> parseElementConst ElemValFloat;
            'D' -> parseElementConst ElemValDouble;
            's' -> parseElementConst ElemValByte;
            'c' -> parseElementConst ElemValClass;
            'e' -> do {
                       typeIdx  <- parseCPoolIndex;
                       constIdx <- parseCPoolIndex;
                       return $ ElemValEnum typeIdx constIdx;
                   };
            '@' -> do {
                       annot <- parseAnnotation;
                       return $ ElemValAnnot annot;
                   };
            '[' -> do {
                       elems <- parseItemsUI16 parseElementValue;
                       return $ ElemValArray elems;
                   };
        }
    };
    
    parseElementConst :: (CPoolIndex -> ElementValue) -> ParseComputation ElementValue;
    parseElementConst cons = do {
        idx <- parseCPoolIndex;
        return $ cons idx
    };

}
