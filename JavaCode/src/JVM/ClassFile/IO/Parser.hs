module JVM.ClassFile.IO.Parser (
    parseClassFile,
-- | Parser for the JVM Class File format
    parseClass,
 --   parseAttributes
) where {

    import JVM.ClassFile.Contents;
    import JVM.ClassFile.Contents.Show;
    import JVM.ClassFile.Contents.Attributes;
    import JVM.ClassFile.Contents.CodeAttribute;
    import JVM.ClassFile.ConstantPool;
    import JVM.ClassFile.ConstantPool.Parser;
    import JVM.ClassFile.IO.ParserUtil;
    import JVM.ClassFile.IO.AttributeNames;
    import JVM.ClassFile.Util.JVMOp;
    import JVM.ClassFile.Contents.Types;
    import JVM.Types.Utils;
    import Control.Monad.State;
    import Control.Monad.Error;
    import Data.Either;
    import Data.Char;
    import Data.Graph.Inductive;
    import JVM.ClassFile.Util.CodeStructurizer;

    import Data.Bits;
    import qualified Data.Map as Map;
        
    -- | The state for parsing
    data CFParseState = CFPS ParseSource Int {-bytes read-} ConstantPool;    
        
    instance SourceContainer CFParseState where {
        getSrc   = do { (CFPS s _ _) <- getState; return s };
        putSrc s = do { (CFPS _ i c) <- getState; putState (CFPS s i c) };
        
        getBytesRead   = do { (CFPS _ i _) <- getState; return i };
        incBytesRead a = do { (CFPS s i c) <- getState; putState (CFPS s (i+a) c) };
    };
        
    instance CPoolContainer CFParseState where {
        getCP   = do { (CFPS _ _ c) <- getState; return c };
        putCP c = do { (CFPS s i _) <- getState; putState (CFPS s i c) };
    };
        
    emptyCFParseState = CFPS [] 0 cpoolEmpty;
        
    test fp = do {
        r <- parseClassFile fp;
        putStrLn $ show r
    }; 
        
    -- | Parse a class from a file     
    parseClassFile :: FilePath -> IO (Either JVMException JVMClass);
    parseClassFile filename = parseFile filename emptyCFParseState parseClass;
        
    -- | Parse a class from classfile data
    parseClass :: JVMOp CFParseState JVMClass;
    parseClass = do {
        magic      <- readUI32;
        minor      <- readUI16;
        major      <- readUI16;                 
        cpoolParse;        
        flags      <- readUI16;
        thisClass  <- parseCPoolIndex;
        superClass <- parseCPoolIndex;
        ifaceNames <- parseIFaces;
        fields     <- parseFields;
        methods    <- parseMethods;
        attrs      <- parseAttributes;
        
        className <- cpoolGetClass thisClass;
        
        superclassName <- if superClass == cpoolNullIndex 
                              then return ""
                              else cpoolGetClass superClass;
        
        classAttrs <- mapM parseClassAttribute attrs;
        
        return JVMClass { 
            className        = className,
            superclassName   = superclassName,
            interfaceNames   = ifaceNames,
            fileVersion      = ClassFileVersion major minor,
            classFlags       = flags,
            classFields      = fields,
            classMethods     = methods,
            classAttributes  = classAttrs
        }
    };

    parseIFaces :: JVMOp CFParseState [JavaClassName];
    parseIFaces = do { 
        ifaceCount   <- readUI16; 
        ifaceIndices <- parseItems ifaceCount parseCPoolIndex;
        mapM cpoolGetClass ifaceIndices
    };

    parseFields :: JVMOp CFParseState [ClassField];
    parseFields = parseItemsUI16 parseField;
    
    parseField :: JVMOp CFParseState ClassField;
    parseField = do {
        flags     <- readUI16;
        nameIndex <- parseCPoolIndex;
        typeIndex <- parseCPoolIndex;
        attrs     <- parseAttributes;
        
        name     <- cpoolGetUTF8 nameIndex;
        typeName <- cpoolGetUTF8 typeIndex;
        
        fieldAttrs <- mapM parseFieldAttribute attrs;
        
        return ClassField {
            fieldFlags      = flags,
            fieldName       = name,
            fieldType       = jtypeFromSig typeName,
            fieldAttributes = fieldAttrs
        }
    };

    parseAttributes :: JVMOp CFParseState [Attribute];
    parseAttributes = parseItemsUI16 parseAttribute;
    
    parseAttribute :: JVMOp CFParseState Attribute;
    parseAttribute = do {
        nameIndex <- parseCPoolIndex;
        size      <- readUI32;
        attrData  <- readSourceElements (fromEnum size);
        name      <- cpoolGetUTF8 nameIndex;
        return $ Attribute name size attrData;
    };

    parseMethods :: JVMOp CFParseState [ClassMethod];
    parseMethods = parseItemsUI16 parseMethod;

    parseMethod :: JVMOp CFParseState ClassMethod;
    parseMethod = do {
        flags     <- readUI16;
        nameIndex <- parseCPoolIndex;
        descIndex <- parseCPoolIndex;
        attrs     <- parseAttributes;
          
        name <- cpoolGetUTF8 nameIndex;
        sig  <- cpoolGetUTF8 descIndex;
          
        methodAttrs <- mapM parseMethodAttribute attrs;
        let (exceptions,aa) = extractExceptions methodAttrs
         in
            return ClassMethod { 
                methodName       = name,
                methodSig        = signatureFromJVMSig sig,
                methodFlags      = flags,
                thrownExceptions = exceptions,
                methodAttributes = aa
            }
    };

    ----------------------------------------------------------------------
    -- Class Attribute Parsers
    ----------------------------------------------------------------------
    
    parseClassAttribute :: Attribute -> JVMOp CFParseState ClassAttribute;
    parseClassAttribute = parseAttr classAttributeParserMap ClassAttribute;

    -- | Map of attribute name to parser function
    classAttributeParserMap :: AttrParserMap ClassAttribute;
    classAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots ClassAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots ClassAnnotations False ),
        (attrName_InnerClasses,                parseInnerClasses ),
        (attrName_EnclosingMethod,             parseEnclosingMethod ),
        (attrName_Synthetic,                   return SyntheticClass ),
        (attrName_Signature,                   parseIdxAttrStr ClassSignature ),
        (attrName_SourceFile,                  parseIdxAttrStr SourceFile ),
        (attrName_Deprecated,                  return DeprecatedClass )    
    ];
    
    parseEnclosingMethod :: JVMOp CFParseState ClassAttribute;
    parseEnclosingMethod = do {
        classIdx  <- parseCPoolIndex;
        methodIdx <- parseCPoolIndex;
        
        refClass   <- cpoolGetClass classIdx;
        (name,typ) <- cpoolGetNameAndType methodIdx; 
        
        return $ EnclosingMethod (MethodRef refClass name (signatureFromJVMSig typ));
    };

    parseInnerClasses :: JVMOp CFParseState ClassAttribute;
    parseInnerClasses = do {
        innerClasses <- parseItemsUI16 parseInnerClass;
        return $ InnerClasses innerClasses;
    };

    parseInnerClass :: JVMOp CFParseState InnerClass;
    parseInnerClass = do {
        innerClassIdx   <- parseCPoolIndex;
        outerClassIdx   <- parseCPoolIndex;
        innerNameIdx    <- parseCPoolIndex;
        innerClassFlags <- readUI16;
        
        innerClass <- cpoolGetClass innerClassIdx; 
        outerClass <- cpoolGetClass outerClassIdx; 
        innerName  <- cpoolGetUTF8  innerNameIdx;
        
        return $ InnerClass innerClass outerClass innerName innerClassFlags;
    };
    
    ----------------------------------------------------------------------
    -- Annotation Parser
    ----------------------------------------------------------------------    
    
    parseAnnots :: (Bool -> [Annotation] -> a) -- constructor
                -> Bool -- whether visible
                -> JVMOp CFParseState  a;
    parseAnnots c v = do {
        annotations <- parseItemsUI16 parseAnnotation;
        return $ c v annotations
    };          
                
    
    parseAnnotation :: JVMOp CFParseState  Annotation;
    parseAnnotation = do {
        typeIndex <- parseCPoolIndex;                    
        elemVals  <- parseItemsUI16 parseNameAndValue;
        
        typeName <- cpoolGetUTF8 typeIndex;
        
        return $ Annotation (jtypeFromSig typeName) elemVals;
    };

    parseNameAndValue :: JVMOp CFParseState  (String,ElementValue);
    parseNameAndValue = do {
        nameIdx <- parseCPoolIndex;
        elemVal <- parseElementValue;
        
        name <- cpoolGetUTF8 nameIdx;
        return (name,elemVal);
    };
    
    
    parseElementValue :: JVMOp CFParseState ElementValue;
    parseElementValue = do {
        tag <- readChar;
        case tag of {
            'B' -> do {
                       idx <- parseCPoolIndex;
                       int <- cpoolGetInt idx;
                       return $ ElemValByte (fromIntegral int)                       
                   };
            'C' -> do {
                       idx <- parseCPoolIndex;
                       int <- cpoolGetInt idx;
                       return $ ElemValChar (chr $ fromIntegral int)                       
                   };
            'S' -> do {
                       idx <- parseCPoolIndex;
                       int <- cpoolGetInt idx;
                       return $ ElemValShort (fromIntegral int)                       
                   };
            'Z' -> do {
                       idx <- parseCPoolIndex;
                       int <- cpoolGetInt idx;
                       return $ ElemValBool (int == 1)                       
                   };
            'I' -> do {
                       idx <- parseCPoolIndex;
                       int <- cpoolGetInt idx;
                       return $ ElemValInt int                       
                   };
            'J' -> do {
                       idx  <- parseCPoolIndex;
                       long <- cpoolGetLong idx;
                       return $ ElemValLong long                       
                   };
            'F' -> do {
                       idx <- parseCPoolIndex;
                       flo <- cpoolGetFloat idx;
                       return $ ElemValFloat flo                       
                   };
            'D' -> do {
                       idx <- parseCPoolIndex;
                       dub <- cpoolGetDouble idx;
                       return $ ElemValDouble dub                       
                   };
            's' -> do {
                       idx <- parseCPoolIndex;
                       str <- cpoolGetUTF8 idx;
                       return $ ElemValString str                       
                   };
            'c' -> do {
                       idx <- parseCPoolIndex;
                       str <- cpoolGetUTF8 idx;
                       return $ ElemValClass $ jtypeFromSig str                       
                   };
            'e' -> do {
                       typeIdx  <- parseCPoolIndex;
                       constIdx <- parseCPoolIndex;
                       
                       typeName <- cpoolGetUTF8 typeIdx;
                       consName <- cpoolGetUTF8 constIdx;
                       return $ ElemValEnum (jtypeFromSig typeName) consName;
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
    
    ----------------------------------------------------------------------
    -- Common Attribute Parsers
    ----------------------------------------------------------------------

    parseIdxAttr :: (CPoolIndex -> a) -> JVMOp CFParseState a;
    parseIdxAttr c = do {
        idx <- parseCPoolIndex;
        return $ c idx;
    };

    parseIdxAttrStr :: (String -> a) -> JVMOp CFParseState a;
    parseIdxAttrStr c = do {
        idx <- parseCPoolIndex;
        str <- cpoolGetUTF8 idx;
        return $ c str;
    };

    type AttrParserMap a = Map.Map String (JVMOp CFParseState a);

    parseAttr :: AttrParserMap a   -- parser map
              -> (Attribute -> a)  -- constructor
              -> Attribute         -- attribute to parse
              -> JVMOp CFParseState a;
    parseAttr m c attr@(Attribute name _ bytes) = do {
        parser <- (Map.lookup name m) `catchError` (\_-> return $ makeRawAttr c attr);
        
        state <- get;        
        putSrc bytes;
        a <- parser;
        put state;

        return a 
    };
    
    makeRawAttr :: (Attribute -> a) -> Attribute -> JVMOp CFParseState a;
    makeRawAttr cons a = return $ cons a;
    
    
    ----------------------------------------------------------------------
    -- Field Attribute Parsers
    ----------------------------------------------------------------------
    
    parseFieldAttribute ::  Attribute -> JVMOp CFParseState FieldAttribute;
    parseFieldAttribute = parseAttr fieldAttributeParserMap FieldAttribute;

    -- | Map of attribute name to parser function
    fieldAttributeParserMap :: Map.Map String (JVMOp CFParseState FieldAttribute);
    fieldAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots FieldAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots FieldAnnotations False ),
        (attrName_Synthetic,                   return SyntheticField ),
        (attrName_Signature,                   do {
                                                   idx <- parseCPoolIndex;
                                                   str <- cpoolGetUTF8 idx;
                                                   return $ FieldSignature (jtypeFromSig str)
                                               }),
        (attrName_ConstantValue,               do {
                                                   idx <- parseCPoolIndex;
                                                   con <- cpoolGetConstant idx;
                                                   return $ ConstantField con
                                               }),
        (attrName_Deprecated,                  return DeprecatedField )    
    ];
    
    ----------------------------------------------------------------------
    -- Method Attribute Parsers
    ----------------------------------------------------------------------
    
    parseMethodAttribute :: Attribute -> JVMOp CFParseState MethodAttribute;
    parseMethodAttribute = parseAttr methodAttributeParserMap MethodAttribute;

    -- | Map of attribute name to parser function
    methodAttributeParserMap :: Map.Map String (JVMOp CFParseState MethodAttribute);
    methodAttributeParserMap = Map.fromList [
        (attrName_RuntimeVisibleAnnotations,   parseAnnots MethodAnnotations True  ),
        (attrName_RuntimeInvisibleAnnotations, parseAnnots MethodAnnotations False ),
        (attrName_Synthetic,                   return SyntheticMethod ),
        (attrName_Signature,                   do {
                                                   idx <- parseCPoolIndex;
                                                   str <- cpoolGetUTF8 idx;
                                                   return $ MethodSignature (signatureFromJVMSig str)
                                               }),
        (attrName_AnnotationDefault,           parseAnnotationDefault ),    
        (attrName_Deprecated,                  return DeprecatedMethod ),
        (attrName_Exceptions,                  parseExceptions ),
        (attrName_Code,                        parseCode ),
        (attrName_RuntimeVisibleParameterAnnotations  , parseParamAnnotations True ),
        (attrName_RuntimeInvisibleParameterAnnotations, parseParamAnnotations False)
    ];
    
    -- extract exceptions from the method attributes
    extractExceptions :: [MethodAttribute] -> ([JavaType],[MethodAttribute]);
    extractExceptions [] = ([],[]);
    extractExceptions ((Exceptions ee):aa) = (ee,aa); 
    extractExceptions (a:aa) = let (ee,rest) = extractExceptions aa
                                in (ee,a:rest);  
    

    parseCode :: JVMOp CFParseState MethodAttribute;
    parseCode = do {
        code <- parseCodeAttribute;
        return $ Code code;    
    };

    parseExceptions :: JVMOp CFParseState MethodAttribute;
    parseExceptions = do {
        excps   <- parseItemsUI16 parseCPoolIndex;
        exNames <- mapM cpoolGetClass excps;
        return $ Exceptions (map jtypeFromSig exNames)
    };
    
    parseAnnotationDefault :: JVMOp CFParseState MethodAttribute;
    parseAnnotationDefault = do {
        elemVal <- parseElementValue;
        return $ AnnotationDefault elemVal;    
    };
    
    parseParamAnnotations :: Bool -> JVMOp CFParseState MethodAttribute;
    parseParamAnnotations v = do {
        numParams <- readUI8;
        annots    <- parseItems numParams (parseItemsUI16 parseAnnotation);
        return $ ParameterAnnotations v annots;
    };

    ----------------------------------------------------------------------
    -- Code Attribute Parser
    ----------------------------------------------------------------------
    

    -- | Parse a code attribute                                   
    parseCodeAttribute :: JVMOp CFParseState CodeAttribute;
    parseCodeAttribute = do {
                             max_stack  <- readUI16;
                             max_locals <- readUI16;                             
                             code       <- parseByteCode;
                             exceptions <- parseExceptionHandlers;
                             attributes <- parseAttributes;

                             return $ incorporateExceptionHandlers CodeAttribute {
                                 maxStack       = max_stack,
                                 maxLocals      = max_locals,
                                 bytecode       = code,
                                 exceptionTable = exceptions,
                                 codeAttributes = attributes
                             };
                         };

    parseByteCode :: JVMOp CFParseState InstructionGraph;
    parseByteCode = do {
                        codeSize <- readUI32;
                        parseInstructions (i2i codeSize) 0 empty
                    };

    parseInstructions :: Int    -- code size
                      -> Int       -- offset
                      -> InstructionGraph 
                      -> JVMOp CFParseState InstructionGraph;
    parseInstructions codeSize addr ig = do {
        beforeBytes <- getBytesRead;
        opcode      <- readUI8;
        (ins,ee,f)  <- parseInstruction opcode addr;
        afterBytes  <- getBytesRead;
      
        -- add nodes on the forward stroke, add edges on the return stroke
        let newAddr = addr + afterBytes - beforeBytes
            edges   = if f then (addr,newAddr,NormalFlow):ee else ee
            ig2     = insNode (addr,ins) ig
          in do {              
              igFinal <- if newAddr >= codeSize 
                            then return ig2
                            else parseInstructions codeSize newAddr ig2;
              return $ insEdges edges igFinal
          }
    };

    returnT :: Instruction -> JVMOp CFParseState (Instruction, [LEdge CodeFlow], Bool) ;
    returnT i = do { return (i,[],True) };

    returnF :: Instruction -> JVMOp CFParseState (Instruction, [LEdge CodeFlow], Bool) ;
    returnF i = do { return (i,[],False) };


    parseInstruction :: Word8  -- opcode
                     -> Int    -- current offset
                     -> JVMOp CFParseState (Instruction,  
                                            [LEdge CodeFlow],  -- branch flows
                                            Bool) ;            -- whether flows to next
    parseInstruction opcode addr = do { case opcode of {
        0x00 -> returnT NoOp;
        0x01 -> returnT PushNull;
        0x02 -> returnT $ PushConstant (ConstInt (-1));
        0x03 -> returnT $ PushConstant (ConstInt 0);
        0x04 -> returnT $ PushConstant (ConstInt 1);
        0x05 -> returnT $ PushConstant (ConstInt 2);
        0x06 -> returnT $ PushConstant (ConstInt 3);
        0x07 -> returnT $ PushConstant (ConstInt 4);
        0x08 -> returnT $ PushConstant (ConstInt 5);
        0x09 -> returnT $ PushConstant (ConstLong 0);
        0x0a -> returnT $ PushConstant (ConstLong 1);
        0x0b -> returnT $ PushConstant (ConstFloat 0);
        0x0c -> returnT $ PushConstant (ConstFloat 1);
        0x0d -> returnT $ PushConstant (ConstFloat 2);
        0x0e -> returnT $ PushConstant (ConstDouble 0);
        0x0f -> returnT $ PushConstant (ConstDouble 1);
        0x10 -> do { value <- readSI8;  returnT $ PushConstant (ConstInt $ i2i value)};
        0x11 -> do { value <- readSI16; returnT $ PushConstant (ConstInt $ i2i value)};
        0x12 -> do { i <- parseCPoolIndex8; c <- cpoolGetConstant i; returnT $ PushConstant c };
        0x13 -> do { i <- parseCPoolIndex;  c <- cpoolGetConstant i; returnT $ PushConstant c };
        0x14 -> do { i <- parseCPoolIndex;  c <- cpoolGetConstant i; returnT $ PushConstant c };
        0x15 -> do { var   <- readUI8;  returnT $ PushVar $ i2i var };
        0x16 -> do { var   <- readUI8;  returnT $ PushVar $ i2i var };
        0x17 -> do { var   <- readUI8;  returnT $ PushVar $ i2i var };
        0x18 -> do { var   <- readUI8;  returnT $ PushVar $ i2i var };
        0x19 -> do { var   <- readUI8;  returnT $ PushVar $ i2i var };
        0x1a -> returnT $ PushVar 0;
        0x1b -> returnT $ PushVar 1;
        0x1c -> returnT $ PushVar 2;
        0x1d -> returnT $ PushVar 3;
        0x1e -> returnT $ PushVar 0;
        0x1f -> returnT $ PushVar 1;
        0x20 -> returnT $ PushVar 2;
        0x21 -> returnT $ PushVar 3;
        0x22 -> returnT $ PushVar 0;
        0x23 -> returnT $ PushVar 1;
        0x24 -> returnT $ PushVar 2;
        0x25 -> returnT $ PushVar 3;
        0x26 -> returnT $ PushVar 0;
        0x27 -> returnT $ PushVar 1;
        0x28 -> returnT $ PushVar 2;
        0x29 -> returnT $ PushVar 3;
        0x2a -> returnT $ PushVar 0;
        0x2b -> returnT $ PushVar 1;
        0x2c -> returnT $ PushVar 2;
        0x2d -> returnT $ PushVar 3;
        0x2e -> returnT PushElement ;
        0x2f -> returnT PushElement ;
        0x30 -> returnT PushElement ;
        0x31 -> returnT PushElement ;
        0x32 -> returnT PushElement ;
        0x33 -> returnT PushElement ;
        0x34 -> returnT PushElement ;
        0x35 -> returnT PushElement ;
        0x36 -> do { var <- readUI8; returnT $ StoreVar $ i2i var };
        0x37 -> do { var <- readUI8; returnT $ StoreVar $ i2i var };
        0x38 -> do { var <- readUI8; returnT $ StoreVar $ i2i var };
        0x39 -> do { var <- readUI8; returnT $ StoreVar $ i2i var };
        0x3a -> do { var <- readUI8; returnT $ StoreVar $ i2i var };
        0x3b -> returnT $ StoreVar 0;
        0x3c -> returnT $ StoreVar 1;
        0x3d -> returnT $ StoreVar 2;
        0x3e -> returnT $ StoreVar 3;
        0x3f -> returnT $ StoreVar 0;
        0x40 -> returnT $ StoreVar 1;
        0x41 -> returnT $ StoreVar 2;
        0x42 -> returnT $ StoreVar 3;
        0x43 -> returnT $ StoreVar 0;
        0x44 -> returnT $ StoreVar 1;
        0x45 -> returnT $ StoreVar 2;
        0x46 -> returnT $ StoreVar 3;
        0x47 -> returnT $ StoreVar 0;
        0x48 -> returnT $ StoreVar 1;
        0x49 -> returnT $ StoreVar 2;
        0x4a -> returnT $ StoreVar 3;
        0x4b -> returnT $ StoreVar 0;
        0x4c -> returnT $ StoreVar 1;
        0x4d -> returnT $ StoreVar 2;
        0x4e -> returnT $ StoreVar 3;
        0x4f -> returnT StoreElement ;
        0x50 -> returnT StoreElement ;
        0x51 -> returnT StoreElement ;
        0x52 -> returnT StoreElement ;
        0x53 -> returnT StoreElement ;
        0x54 -> returnT StoreElement ;
        0x55 -> returnT StoreElement ;
        0x56 -> returnT StoreElement ;
        0x57 -> returnT $ Pop 1 ;
        0x58 -> returnT $ Pop 2 ;
        0x59 -> returnT $ Dup 1 0 ;
        0x5a -> returnT $ Dup 1 1 ;
        0x5b -> returnT $ Dup 1 2 ;
        0x5c -> returnT $ Dup 2 0 ;
        0x5d -> returnT $ Dup 2 1 ;
        0x5e -> returnT $ Dup 2 2 ;
        0x5f -> returnT Swap    ;
        0x60 -> returnT Add    ;
        0x61 -> returnT Add    ;
        0x62 -> returnT Add    ;
        0x63 -> returnT Add    ;
        0x64 -> returnT Sub    ;
        0x65 -> returnT Sub    ;
        0x66 -> returnT Sub    ;
        0x67 -> returnT Sub    ;
        0x68 -> returnT Mul    ;
        0x69 -> returnT Mul    ;
        0x6a -> returnT Mul    ;
        0x6b -> returnT Mul    ;
        0x6c -> returnT Div    ;
        0x6d -> returnT Div    ;
        0x6e -> returnT Div    ;
        0x6f -> returnT Div    ;
        0x70 -> returnT Rem    ;
        0x71 -> returnT Rem    ;
        0x72 -> returnT Rem    ;
        0x73 -> returnT Rem    ;
        0x74 -> returnT Neg    ;
        0x75 -> returnT Neg    ;
        0x76 -> returnT Neg    ;
        0x77 -> returnT Neg    ;
        0x78 -> returnT ShiftLeft ;
        0x79 -> returnT ShiftLeft ;
        0x7a -> returnT ShiftRight  ;
        0x7b -> returnT ShiftRight  ;
        0x7c -> returnT ShiftRightU ;
        0x7d -> returnT ShiftRightU ;
        0x7e -> returnT And;
        0x7f -> returnT And;
        0x80 -> returnT Or;
        0x81 -> returnT Or;
        0x82 -> returnT Xor;
        0x83 -> returnT Xor;
        0x84 -> do { var <- readUI8; increment <- readSI8; returnT $ Increment (i2i var) (i2i increment) };
        0x85 -> returnT $ Convert JLong;
        0x86 -> returnT $ Convert JFloat;
        0x87 -> returnT $ Convert JDouble;
        0x88 -> returnT $ Convert JInt;
        0x89 -> returnT $ Convert JFloat;
        0x8a -> returnT $ Convert JDouble;
        0x8b -> returnT $ Convert JInt;
        0x8c -> returnT $ Convert JLong;
        0x8d -> returnT $ Convert JDouble;
        0x8e -> returnT $ Convert JInt;
        0x8f -> returnT $ Convert JLong;
        0x90 -> returnT $ Convert JFloat;
        0x91 -> returnT $ Convert JByte;
        0x92 -> returnT $ Convert JChar;
        0x93 -> returnT $ Convert JShort;
        0x94 -> returnT Compare;
        0x95 -> returnT CompareL;
        0x96 -> returnT CompareG;
        0x97 -> returnT CompareL;
        0x98 -> returnT CompareG;
        0x99 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfEQ0)],True) };
        0x9a -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfNE0)],True) };
        0x9b -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfLT0)],True) };
        0x9c -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfGE0)],True) };
        0x9d -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfGT0)],True) };
        0x9e -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfLE0)],True) };
        
        0x9f -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfEQ)],True) };
        0xa0 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfNE)],True) };
        0xa1 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfLT)],True) };
        0xa2 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfGE)],True) };
        0xa3 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfGT)],True) };
        0xa4 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfLE)],True) };
        0xa5 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfEQ)],True) };
        0xa6 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfNE)],True) };
        
        0xa7 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),NormalFlow)],False) };
        0xa8 -> do { offset <- readSI16; return $ (SubCall,[(addr,addr + (i2i offset),SubroutineCall)],True) };
        0xa9 -> do { var <- readUI8; returnF $ SubRet $ i2i var };
        0xaa -> parseTableSwitch addr;
        0xab -> parseLookupSwitch addr;
        
        0xac -> returnF Return;
        0xad -> returnF Return;
        0xae -> returnF Return;
        0xaf -> returnF Return;
        0xb0 -> returnF Return;
        0xb1 -> returnF Return;
        
        0xb2 -> do { i <- parseCPoolIndex; fieldRef <- cpoolGetFieldRef i; returnT $ GetStatic fieldRef }; 
        0xb3 -> do { i <- parseCPoolIndex; fieldRef <- cpoolGetFieldRef i; returnT $ PutStatic fieldRef }; 
        0xb4 -> do { i <- parseCPoolIndex; fieldRef <- cpoolGetFieldRef i; returnT $ GetField  fieldRef }; 
        0xb5 -> do { i <- parseCPoolIndex; fieldRef <- cpoolGetFieldRef i; returnT $ PutField  fieldRef };  
        
        0xb6 -> do { i <- parseCPoolIndex; methRef <- cpoolGetMethodRef i; returnT $ InvokeVirtual methRef };
        0xb7 -> do { i <- parseCPoolIndex; methRef <- cpoolGetMethodRef i; returnT $ InvokeSpecial methRef };
        0xb8 -> do { i <- parseCPoolIndex; methRef <- cpoolGetMethodRef i; returnT $ InvokeStatic  methRef };
        0xb9 -> do { i <- parseCPoolIndex; argCount <- readUI8; readUI8; methRef <- cpoolGetMethodRef i; 
                     returnT $ InvokeInterface methRef };

        0xbb -> do { i <- parseCPoolIndex; name <- cpoolGetClass i; returnT $ New (jvmFormatToClassName name) };
        
        0xbc -> do { atype <- readUI8;  returnT $ NewArray (Primitive $ primitiveArrayTypeFromCode atype) 1 };
        0xbd -> do { i <- parseCPoolIndex; name <- cpoolGetClass i; returnT $ NewArray (jtypeFromSig name) 1  }; 
        
        0xbe -> returnT ArrayLength;
        0xbf -> returnF Throw;
        0xc0 -> do { i <- parseCPoolIndex; c <- cpoolGetClass i; returnT $ CheckCast (jtypeFromClassName c) };
        0xc1 -> do { i <- parseCPoolIndex; c <- cpoolGetClass i; returnT $ InstanceOf (jtypeFromClassName c) };
        0xc2 -> returnT MonitorEnter;
        0xc3 -> returnT MonitorExit;
        0xc4 -> parseWide;
        
        0xc5 -> do { 
                    i    <- parseCPoolIndex; 
                    dims <- readUI8; 
                    typ  <- cpoolGetClass i;
                    returnT $ NewArray (dropArrayDims dims $ jtypeFromSig typ) (i2i dims) 
                };
        
        0xc6 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfNull   )],True) };
        0xc7 -> do { offset <- readSI16; return $ (Branch, [(addr,addr + (i2i offset),Conditional IfNotNull)],True) };
        0xc8 -> do { offset <- readSI32; return $ (Branch, [(addr,addr + (i2i offset),NormalFlow)],False) };
        0xc9 -> do { offset <- readSI32; return $ (SubCall,[(addr,addr + (i2i offset),SubroutineCall)],True) };
        otherwise -> fail ("unknown opcode " ++ (show opcode));
        
    }} `catchError` (\msg-> throwError $ msg ++ " - while parsing opcode " ++ (show opcode)) ;

    parseWide :: JVMOp CFParseState (Instruction, [LEdge CodeFlow], Bool);
    parseWide = do {
                    opcode <- readUI8;
                    case opcode of {
                        0x15 -> do { var <- readUI16; returnT $ PushVar $ i2i var };
                        0x16 -> do { var <- readUI16; returnT $ PushVar $ i2i var };
                        0x17 -> do { var <- readUI16; returnT $ PushVar $ i2i var };
                        0x18 -> do { var <- readUI16; returnT $ PushVar $ i2i var };
                        0x19 -> do { var <- readUI16; returnT $ PushVar $ i2i var };
                        0x36 -> do { var <- readUI16; returnT $ StoreVar $ i2i var };
                        0x37 -> do { var <- readUI16; returnT $ StoreVar $ i2i var };
                        0x38 -> do { var <- readUI16; returnT $ StoreVar $ i2i var };
                        0x39 -> do { var <- readUI16; returnT $ StoreVar $ i2i var };
                        0x3a -> do { var <- readUI16; returnT $ StoreVar $ i2i var };
                        0x84 -> do { var <- readUI16; increment <- readSI16; returnT $ Increment (i2i var) (i2i increment) };
                        0xa9 -> do { var <- readUI16; returnF $ SubRet $ i2i var };
                        otherwise -> fail ("unknown wide opcode" ++ (show opcode));
                    };
                };

    parseLookupSwitch :: Int -> JVMOp CFParseState (Instruction, [LEdge CodeFlow], Bool);
    parseLookupSwitch addr = do { 
                                 padding   <- skipSwitchPadding addr; 
                                 defOffset <- readSI32;
                                 numPair   <- readUI32;
                                 cases     <- parseItems (fromEnum numPair) 
                                                $ do { 
                                                      val    <- readSI32; 
                                                      offset <- readSI32; 
                                                      return (addr, addr + (i2i offset), Case val) 
                                                  };
                                                  
                                 return $ (Switch, (addr,addr + (i2i defOffset),DefaultCase):cases, False)
                            };

    parseTableSwitch :: Int -> JVMOp CFParseState (Instruction, [LEdge CodeFlow], Bool);
    parseTableSwitch addr = do { 
                                padding   <- skipSwitchPadding addr; 
                                defOffset <- readSI32;
                                low       <- readSI32;
                                high      <- readSI32;
                                offsets   <- parseItems (high-low+1) readSI32;
                                
                                let pairs = zip [low .. high] $ map (addr +) $ map i2i offsets
                                    cases = map (\(v,off)->(addr,off,Case v)) pairs
                                 in
                                   return $ (Switch, (addr,addr + (i2i defOffset),DefaultCase):cases, False)
                            };

    primitiveArrayTypeFromCode :: Word8 -> PrimitiveType;                
    primitiveArrayTypeFromCode code = case code of {
                                          4  -> JBoolean;
                                          5  -> JChar;
                                          6  -> JFloat;
                                          7  -> JDouble;
                                          8  -> JByte;
                                          9  -> JShort;
                                          10 -> JInt;
                                          11 -> JLong;
                                      };

    --Skip the padding after a Table/LookupSwitch opcode
    skipSwitchPadding :: Int -> JVMOp CFParseState Int {-Num bytes read-};
    skipSwitchPadding addr = do {
                                 case (addr `mod` 4) of {
                                     0 -> do { readUI8; readUI8; readUI8; return 3 };
                                     1 -> do { readUI8; readUI8; return 2 };
                                     2 -> do { readUI8; return 1 };
                                     3 -> do { return 0 };
                                 };
                             };

    parseExceptionHandlers :: JVMOp CFParseState [ExceptionTableEntry];
    parseExceptionHandlers = do {
                                 exceptionCount <- readUI16;
                                 parseItems exceptionCount parseExceptionHandler;
                             };

    parseExceptionHandler :: JVMOp CFParseState ExceptionTableEntry;
    parseExceptionHandler = do {
                                start     <- readUI16;
                                end       <- readUI16;
                                handler   <- readUI16;
                                typeIndex <- parseCPoolIndex;
                                
                                exType <- if typeIndex == 0
                                            then return "java/lang/Throwable"
                                            else cpoolGetClass typeIndex;
                                
                                return (ExceptionTableEntry 
                                           (i2i start)
                                           (i2i end  )
                                           (i2i handler)
                                           (jvmFormatToClassName exType));
                            } `catchError` (\msg-> throwError $ msg ++ " - while parsing ExceptionHandler");
}
