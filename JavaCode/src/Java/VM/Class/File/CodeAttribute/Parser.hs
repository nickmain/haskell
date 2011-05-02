-- | Parser for the Code attribute
module Java.VM.Class.File.CodeAttribute.Parser (
    parseCodeAttribute,
    ParseSource,
    ParseResult(..)
) where {

    import Java.VM.Class.File.CodeAttribute;
    import Java.VM.Class.File.CodeAttribute.Opcodes;
    import Java.VM.Class.File.IO.ParserUtil;
    import Java.VM.Class.File;
    import Java.VM.Class.File.ConstantPool.Parser;
    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.IO.AttributeParser;
    import Data.Bits(testBit);
    import Data.Word;
    --import Debug.Trace(trace);
        
    -- | Parse a code attribute                                   
    parseCodeAttribute :: ParseComputation CodeAttribute;
    parseCodeAttribute = do {
                             max_stack  <- readUI16;
                             max_locals <- readUI16;                             
                             code       <- parseCode;
                             exceptions <- parseExceptions;
                             attributes <- parseAttributes;

                             return CodeAttribute {
                                 maxStack       = max_stack,
                                 maxLocals      = max_locals,
                                 bytecode       = code,
                                 exceptionTable = exceptions,
                                 codeAttributes = attributes
                             };
                         };

    parseCode :: ParseComputation [Instruction];
    parseCode = do {
                    codeSize <- readUI32;
                    code     <- readSourceElements (fromEnum codeSize);
                    case (parse code $ do { parseInstructions codeSize 0 }) of {
                        (ParseSuccess c) -> return c;
                        (ParseFailure s) -> fail s;
                    };
                };

    parseInstructions :: Word32 {-code size-} -> InstructionAddress -> ParseComputation [Instruction];
    parseInstructions codeSize addr = do {
                                          opcode      <- readUI8;
                                          instruction <- parseInstruction opcode addr;
                                          let newAddr = addr + (sizeofInstruction instruction addr) in
                                              if (i2i newAddr) >= codeSize then return [instruction]
                                              else do {
                                                  restOfInstructions <- parseInstructions codeSize newAddr;
                                                  return $ instruction:restOfInstructions;
                                              };
                                      };

    parseInstruction :: Word8 {-opcode-} -> InstructionAddress -> ParseComputation Instruction;
    parseInstruction opcode addr = do { case opcode of {
        0x00 -> return NOP;
        0x01 -> return ACONST_NULL;
        0x02 -> return ICONST_M1;
        0x03 -> return ICONST_0;
        0x04 -> return ICONST_1;
        0x05 -> return ICONST_2;
        0x06 -> return ICONST_3;
        0x07 -> return ICONST_4;
        0x08 -> return ICONST_5;
        0x09 -> return LCONST_0;
        0x0a -> return LCONST_1;
        0x0b -> return FCONST_0;
        0x0c -> return FCONST_1;
        0x0d -> return FCONST_2;
        0x0e -> return DCONST_0;
        0x0f -> return DCONST_1;
        0x10 -> do { value <- readSI8;  return (BIPUSH value) };
        0x11 -> do { value <- readSI16; return (SIPUSH value) };
        0x12 -> do { index <- parseCPoolIndex8; return (LDC index) };
        0x13 -> do { index <- parseCPoolIndex;  return (LDC_W  index) };
        0x14 -> do { index <- parseCPoolIndex;  return (LDC2_W index) };
        0x15 -> do { var   <- readUI8;  return (ILOAD $ i2i var) };
        0x16 -> do { var   <- readUI8;  return (LLOAD $ i2i var) };
        0x17 -> do { var   <- readUI8;  return (FLOAD $ i2i var) };
        0x18 -> do { var   <- readUI8;  return (DLOAD $ i2i var) };
        0x19 -> do { var   <- readUI8;  return (ALOAD $ i2i var) };
        0x1a -> return ILOAD_0;
        0x1b -> return ILOAD_1;
        0x1c -> return ILOAD_2;
        0x1d -> return ILOAD_3;
        0x1e -> return LLOAD_0;
        0x1f -> return LLOAD_1;
        0x20 -> return LLOAD_2;
        0x21 -> return LLOAD_3;
        0x22 -> return FLOAD_0;
        0x23 -> return FLOAD_1;
        0x24 -> return FLOAD_2;
        0x25 -> return FLOAD_3;
        0x26 -> return DLOAD_0;
        0x27 -> return DLOAD_1;
        0x28 -> return DLOAD_2;
        0x29 -> return DLOAD_3;
        0x2a -> return ALOAD_0;
        0x2b -> return ALOAD_1;
        0x2c -> return ALOAD_2;
        0x2d -> return ALOAD_3;
        0x2e -> return IALOAD ;
        0x2f -> return LALOAD ;
        0x30 -> return FALOAD ;
        0x31 -> return DALOAD ;
        0x32 -> return AALOAD ;
        0x33 -> return BALOAD ;
        0x34 -> return CALOAD ;
        0x35 -> return SALOAD ;
        0x36 -> do { var <- readUI8; return (ISTORE $ i2i var) };
        0x37 -> do { var <- readUI8; return (LSTORE $ i2i var) };
        0x38 -> do { var <- readUI8; return (FSTORE $ i2i var) };
        0x39 -> do { var <- readUI8; return (DSTORE $ i2i var) };
        0x3a -> do { var <- readUI8; return (ASTORE $ i2i var) };
        0x3b -> return ISTORE_0;
        0x3c -> return ISTORE_1;
        0x3d -> return ISTORE_2;
        0x3e -> return ISTORE_3;
        0x3f -> return LSTORE_0;
        0x40 -> return LSTORE_1;
        0x41 -> return LSTORE_2;
        0x42 -> return LSTORE_3;
        0x43 -> return FSTORE_0;
        0x44 -> return FSTORE_1;
        0x45 -> return FSTORE_2;
        0x46 -> return FSTORE_3;
        0x47 -> return DSTORE_0;
        0x48 -> return DSTORE_1;
        0x49 -> return DSTORE_2;
        0x4a -> return DSTORE_3;
        0x4b -> return ASTORE_0;
        0x4c -> return ASTORE_1;
        0x4d -> return ASTORE_2;
        0x4e -> return ASTORE_3;
        0x4f -> return IASTORE ;
        0x50 -> return LASTORE ;
        0x51 -> return FASTORE ;
        0x52 -> return DASTORE ;
        0x53 -> return AASTORE ;
        0x54 -> return BASTORE ;
        0x55 -> return CASTORE ;
        0x56 -> return SASTORE ;
        0x57 -> return POP     ;
        0x58 -> return POP2    ;
        0x59 -> return DUP     ;
        0x5a -> return DUP_X1  ;
        0x5b -> return DUP_X2  ;
        0x5c -> return DUP2    ;
        0x5d -> return DUP2_X1 ;
        0x5e -> return DUP2_X2 ;
        0x5f -> return SWAP    ;
        0x60 -> return IADD    ;
        0x61 -> return LADD    ;
        0x62 -> return FADD    ;
        0x63 -> return DADD    ;
        0x64 -> return ISUB    ;
        0x65 -> return LSUB    ;
        0x66 -> return FSUB    ;
        0x67 -> return DSUB    ;
        0x68 -> return IMUL    ;
        0x69 -> return LMUL    ;
        0x6a -> return FMUL    ;
        0x6b -> return DMUL    ;
        0x6c -> return IDIV    ;
        0x6d -> return LDIV    ;
        0x6e -> return FDIV    ;
        0x6f -> return DDIV    ;
        0x70 -> return IREM    ;
        0x71 -> return LREM    ;
        0x72 -> return FREM    ;
        0x73 -> return DREM    ;
        0x74 -> return INEG    ;
        0x75 -> return LNEG    ;
        0x76 -> return FNEG    ;
        0x77 -> return DNEG    ;
        0x78 -> return ISHL    ;
        0x79 -> return LSHL    ;
        0x7a -> return ISHR    ;
        0x7b -> return LSHR    ;
        0x7c -> return IUSHR   ;
        0x7d -> return LUSHR   ;
        0x7e -> return IAND    ;
        0x7f -> return LAND    ;
        0x80 -> return IOR     ;
        0x81 -> return LOR     ;
        0x82 -> return IXOR    ;
        0x83 -> return LXOR    ;
        0x84 -> do { var <- readUI8; increment <- readSI8; return ( IINC (i2i var) (i2i increment)) };
        0x85 -> return I2L  ;
        0x86 -> return I2F  ;
        0x87 -> return I2D  ;
        0x88 -> return L2I  ;
        0x89 -> return L2F  ;
        0x8a -> return L2D  ;
        0x8b -> return F2I  ;
        0x8c -> return F2L  ;
        0x8d -> return F2D  ;
        0x8e -> return D2I  ;
        0x8f -> return D2L  ;
        0x90 -> return D2F  ;
        0x91 -> return I2B  ;
        0x92 -> return I2C  ;
        0x93 -> return I2S  ;
        0x94 -> return LCMP ;
        0x95 -> return FCMPL;
        0x96 -> return FCMPG;
        0x97 -> return DCMPL;
        0x98 -> return DCMPG;
        0x99 -> do { offset <- readSI16; return (IFEQ      offset) };
        0x9a -> do { offset <- readSI16; return (IFNE      offset) };
        0x9b -> do { offset <- readSI16; return (IFLT      offset) };
        0x9c -> do { offset <- readSI16; return (IFGE      offset) };
        0x9d -> do { offset <- readSI16; return (IFGT      offset) };
        0x9e -> do { offset <- readSI16; return (IFLE      offset) };
        0x9f -> do { offset <- readSI16; return (IF_ICMPEQ offset) };
        0xa0 -> do { offset <- readSI16; return (IF_ICMPNE offset) };
        0xa1 -> do { offset <- readSI16; return (IF_ICMPLT offset) };
        0xa2 -> do { offset <- readSI16; return (IF_ICMPGE offset) };
        0xa3 -> do { offset <- readSI16; return (IF_ICMPGT offset) };
        0xa4 -> do { offset <- readSI16; return (IF_ICMPLE offset) };
        0xa5 -> do { offset <- readSI16; return (IF_ACMPEQ offset) };
        0xa6 -> do { offset <- readSI16; return (IF_ACMPNE offset) };
        0xa7 -> do { offset <- readSI16; return (GOTO      offset) };
        0xa8 -> do { offset <- readSI16; return (JSR       offset) };
        0xa9 -> do { var <- readUI8; return (RET $ i2i var) };
        0xaa -> parseTableSwitch addr;
        0xab -> parseLookupSwitch addr;
        0xac -> return IRETURN;
        0xad -> return LRETURN;
        0xae -> return FRETURN;
        0xaf -> return DRETURN;
        0xb0 -> return ARETURN;
        0xb1 -> return RETURN ;
        0xb2 -> do { index <- parseCPoolIndex; return (GETSTATIC index) }; 
        0xb3 -> do { index <- parseCPoolIndex; return (PUTSTATIC index) }; 
        0xb4 -> do { index <- parseCPoolIndex; return (GETFIELD  index) }; 
        0xb5 -> do { index <- parseCPoolIndex; return (PUTFIELD  index) }; 
        0xb6 -> do { index <- parseCPoolIndex; return (INVOKEVIRTUAL index) };
        0xb7 -> do { index <- parseCPoolIndex; return (INVOKESPECIAL index) };
        0xb8 -> do { index <- parseCPoolIndex; return (INVOKESTATIC  index) };
        0xb9 -> do { index <- parseCPoolIndex; argCount <- readUI8; readUI8; return (INVOKEINTERFACE index argCount) };
        0xbb -> do { index <- parseCPoolIndex; return (NEW index) };
        0xbc -> do { atype <- readUI8;  return (NEWARRAY (primitiveArrayTypeFromCode atype)) };
        0xbd -> do { index <- parseCPoolIndex; return (ANEWARRAY index) };
        0xbe -> return ARRAYLENGTH;
        0xbf -> return ATHROW     ;
        0xc0 -> do { index <- parseCPoolIndex; return (CHECKCAST  index) };
        0xc1 -> do { index <- parseCPoolIndex; return (INSTANCEOF index) };
        0xc2 -> return MONITORENTER;
        0xc3 -> return MONITOREXIT ;
        0xc4 -> parseWide;           ;
        0xc5 -> do { index  <- parseCPoolIndex; dims <- readUI8; return (MULTIANEWARRAY index dims) };
        0xc6 -> do { offset <- readSI16; return (IFNULL    offset) };
        0xc7 -> do { offset <- readSI16; return (IFNONNULL offset) };
        0xc8 -> do { offset <- readSI32; return (GOTO_W offset) };
        0xc9 -> do { offset <- readSI32; return (JSR_W  offset) };
        otherwise -> fail ("unknown opcode " ++ (show opcode));
    }};

    parseWide :: ParseComputation Instruction;
    parseWide = do {
                    opcode <- readUI8;
                    case opcode of {
                        0x15 -> do { var <- readUI16; return (WIDE(ILOAD  var)) };
                        0x16 -> do { var <- readUI16; return (WIDE(LLOAD  var)) };
                        0x17 -> do { var <- readUI16; return (WIDE(FLOAD  var)) };
                        0x18 -> do { var <- readUI16; return (WIDE(DLOAD  var)) };
                        0x19 -> do { var <- readUI16; return (WIDE(ALOAD  var)) };
                        0x36 -> do { var <- readUI16; return (WIDE(ISTORE var)) };
                        0x37 -> do { var <- readUI16; return (WIDE(LSTORE var)) };
                        0x38 -> do { var <- readUI16; return (WIDE(FSTORE var)) };
                        0x39 -> do { var <- readUI16; return (WIDE(DSTORE var)) };
                        0x3a -> do { var <- readUI16; return (WIDE(ASTORE var)) };
                        0x84 -> do { var <- readUI16; increment <- readSI16; return (WIDE(IINC var increment)) };
                        0xa9 -> do { var <- readUI16; return (WIDE(RET var)) };
                        otherwise -> fail ("unknown wide opcode" ++ (show opcode));
                    };
                };

    parseLookupSwitch :: InstructionAddress -> ParseComputation Instruction;
    parseLookupSwitch addr = do { 
                                 padding   <- skipSwitchPadding addr; 
                                 defOffset <- readSI32;
                                 numPair   <- readUI32;
                                 cases     <- parseItems (fromEnum numPair) $ do { val <- readSI32; offset <- readSI32; return (val,offset) };
                                 return (LOOKUPSWITCH padding defOffset numPair cases);
                            };

    parseTableSwitch :: InstructionAddress -> ParseComputation Instruction;
    parseTableSwitch addr = do { 
                                padding   <- skipSwitchPadding addr; 
                                defOffset <- readSI32;
                                low       <- readSI32;
                                high      <- readSI32;
                                offsets   <- parseItems (high-low+1) readSI32;
                                return (TABLESWITCH padding defOffset low high offsets);
                            };

    primitiveArrayTypeFromCode :: Word8 -> PrimitiveArrayType;                
    primitiveArrayTypeFromCode code = case code of {
                                          4  -> BooleanArray;
                                          5  -> CharArray;
                                          6  -> FloatArray;
                                          7  -> DoubleArray;
                                          8  -> ByteArray;
                                          9  -> ShortArray;
                                          10 -> IntArray;
                                          11 -> LongArray;
                                      };

    --Skip the padding after a Table/LookupSwitch opcode
    skipSwitchPadding :: InstructionAddress -> ParseComputation Int {-Num bytes read-};
    skipSwitchPadding addr = do {
                                 case (addr `mod` 4) of {
                                     0 -> do { readUI8; readUI8; readUI8; return 3 };
                                     1 -> do { readUI8; readUI8; return 2 };
                                     2 -> do { readUI8; return 1 };
                                     3 -> do { return 0 };
                                 };
                             };

    parseExceptions :: ParseComputation [ExceptionTableEntry];
    parseExceptions = do {
                          exceptionCount <- readUI16;
                          parseItems exceptionCount parseException;
                      };

    parseException :: ParseComputation ExceptionTableEntry;
    parseException = do {
                         start     <- readUI16;
                         end       <- readUI16;
                         handler   <- readUI16;
                         typeIndex <- parseCPoolIndex;
                         return (ExceptionTableEntry start end handler typeIndex);
                     };
}