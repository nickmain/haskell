-- | Parser for the JVM Class File format
module Java.VM.Class.File.IO.Parser (
    parseClassFile,
    parseClass,
    ParseSource,
    ParseResult(..),
    parseAttributes
) where {

    import Java.VM.Class.File;
    import Java.VM.Class.File.ConstantPool;
    import Java.VM.Class.File.ConstantPool.Parser;
    import Java.VM.Class.File.IO.ParserUtil;
    import Java.VM.Class.File.IO.AttributeNames;
    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.IO.AttributeParser;
    import Java.VM.Class.File.CodeAttribute.Parser;
    import Data.Bits;
    import qualified Data.Map as Map;
        
    -- | Parse a class from a file    
    parseClassFile :: FilePath -> IO (ParseResult ClassFile);
    parseClassFile filename = parseFile filename classParser;
        
    -- | Parse a class from classfile data
    parseClass :: ParseSource -> ParseResult ClassFile;   
    parseClass [] = ParseFailure "Empty class file";
    parseClass cs = parse cs classParser;
    

    classParser :: ParseComputation ClassFile;
    classParser = do {
        magic      <- readUI32;
        minor      <- readUI16;
        major      <- readUI16;         
        cp         <- cpoolParse;
        flags      <- readUI16;
        thisClass  <- parseCPoolIndex;
        superClass <- parseCPoolIndex;
        ifIndices  <- parseIFaceIndices;
        fields     <- parseFields  cp;
        methods    <- parseMethods cp;
        attrs      <- parseAttributes;
        
        return ClassFile { 
            magicNumber       = magic, 
            minorVersion      = minor,
            majorVersion      = major, 
            constantPool      = cp,
            classFlags        = flags,
            classIndex        = thisClass,
            superClassIndex   = superClass,
            interfaceIndices  = ifIndices,
            fieldInfos        = fields,
            methodInfos       = methods,                            
            classAttributes   = map (parseClassAttribute cp)attrs
        };
    };

    parseMethods :: ConstantPool -> ParseComputation [MethodInfo];
    parseMethods cp = parseItemsUI16 (parseMethod cp);

    parseMethod :: ConstantPool -> ParseComputation MethodInfo;
    parseMethod cp = do {
        flags     <- readUI16;
        nameIndex <- parseCPoolIndex;
        descIndex <- parseCPoolIndex;
        attrs     <- parseAttributes;
          
        return MethodInfo {                          
            methodFlags      = flags,                     
            methodNameIndex  = nameIndex,
            methodDescIndex  = descIndex,
            methodAttributes = map (parseMethodAttribute cp) attrs
        };
    };

    parseFields :: ConstantPool -> ParseComputation [FieldInfo];
    parseFields cp = parseItemsUI16 (parseField cp);
    
    parseField :: ConstantPool -> ParseComputation FieldInfo;
    parseField cp = do {
        flags     <- readUI16;
        nameIndex <- parseCPoolIndex;
        typeIndex <- parseCPoolIndex;
        attrs     <- parseAttributes;
        
        return FieldInfo {
            fieldFlags      = flags,
            fieldNameIndex  = nameIndex,
            fieldTypeIndex  = typeIndex,
            fieldAttributes = map (parseFieldAttribute cp) attrs
        };
    };

    parseIFaceIndices :: ParseComputation [CPoolIndex];
    parseIFaceIndices = do { 
        ifaceCount <- readUI16; 
        parseItems ifaceCount parseCPoolIndex 
    };
    
    ----------------------------------------------------------------------
    -- Class Attribute Parsers
    ----------------------------------------------------------------------
    
    parseClassAttribute ::  ConstantPool -> Attribute -> ClassAttribute;
    parseClassAttribute = parseAttr classAttributeParserMap ClassAttribute;

    -- | Map of attribute name to parser function
    classAttributeParserMap :: Map.Map String (ParseComputation ClassAttribute);
    classAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots ClassAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots ClassAnnotations False ),
        (attrName_InnerClasses,                parseInnerClasses ),
        (attrName_EnclosingMethod,             parseEnclosingMethod ),
        (attrName_Synthetic,                   return SyntheticClass ),
        (attrName_Signature,                   parseIdxAttr ClassSignature ),
        (attrName_SourceFile,                  parseIdxAttr SourceFile ),
        (attrName_Deprecated,                  return DeprecatedClass )    
    ];
    
    parseEnclosingMethod :: ParseComputation ClassAttribute;
    parseEnclosingMethod = do {
        classIdx  <- parseCPoolIndex;
        methodIdx <- parseCPoolIndex;
        return $ EnclosingMethod classIdx methodIdx;
    };

    parseInnerClasses :: ParseComputation ClassAttribute;
    parseInnerClasses = do {
        innerClasses <- parseItemsUI16 parseInnerClass;
        return $ InnerClasses innerClasses;
    };

    parseInnerClass :: ParseComputation InnerClass;
    parseInnerClass = do {
        innerClassIdx   <- parseCPoolIndex;
        outerClassIdx   <- parseCPoolIndex;
        innerNameIdx    <- parseCPoolIndex;
        innerClassFlags <- readUI16;
        return $ InnerClass innerClassIdx outerClassIdx innerNameIdx innerClassFlags;
    };
    
    ----------------------------------------------------------------------
    -- Field Attribute Parsers
    ----------------------------------------------------------------------
    
    parseFieldAttribute ::  ConstantPool -> Attribute -> FieldAttribute;
    parseFieldAttribute = parseAttr fieldAttributeParserMap FieldAttribute;

    -- | Map of attribute name to parser function
    fieldAttributeParserMap :: Map.Map String (ParseComputation FieldAttribute);
    fieldAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots FieldAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots FieldAnnotations False ),
        (attrName_Synthetic,                   return SyntheticField ),
        (attrName_Signature,                   parseIdxAttr FieldSignature ),
        (attrName_ConstantValue,               parseIdxAttr ConstantValue ),
        (attrName_Deprecated,                  return DeprecatedField )    
    ];
    
    ----------------------------------------------------------------------
    -- Method Attribute Parsers
    ----------------------------------------------------------------------
    
    parseMethodAttribute ::  ConstantPool -> Attribute -> MethodAttribute;
    parseMethodAttribute = parseAttr methodAttributeParserMap MethodAttribute;

    -- | Map of attribute name to parser function
    methodAttributeParserMap :: Map.Map String (ParseComputation MethodAttribute);
    methodAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots MethodAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots MethodAnnotations False ),
        (attrName_Synthetic,                   return SyntheticMethod ),
        (attrName_Signature,                   parseIdxAttr MethodSignature ),
        (attrName_AnnotationDefault,           parseAnnotationDefault ),    
        (attrName_Deprecated,                  return DeprecatedMethod ),
        (attrName_Exceptions,                  parseExceptions ),
        (attrName_Code,                        parseCode ),
        (attrName_RuntimeVisibleParameterAnnotations  , parseParamAnnotations True ),
        (attrName_RuntimeInvisibleParameterAnnotations, parseParamAnnotations False)
    ];

    parseCode :: ParseComputation MethodAttribute;
    parseCode = do {
        code <- parseCodeAttribute;
        return $ Code code;    
    };

    parseExceptions :: ParseComputation MethodAttribute;
    parseExceptions = do {
        excps <- parseItemsUI16 parseCPoolIndex;
        return $ Exceptions excps;
    };
    
    parseAnnotationDefault :: ParseComputation MethodAttribute;
    parseAnnotationDefault = do {
        elemVal <- parseElementValue;
        return $ AnnotationDefault elemVal;    
    };
    
    parseParamAnnotations :: Bool -> ParseComputation MethodAttribute;
    parseParamAnnotations v = do {
        numParams <- readUI8;
        annots    <- parseItems numParams (parseItemsUI16 parseAnnotation);
        return $ ParameterAnnotations v annots;
    };

}