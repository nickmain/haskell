module Java.VM.Class.File.CodeAttribute.Writer( writeCodeAttribute ) where {

    import Java.VM.Class.File;
    import Java.VM.Class.File.CodeAttribute;
    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.IO.WriterUtil;
    import Java.VM.Class.File.IO.ParserUtil hiding (i2i);
    import Java.VM.Class.File.ConstantPool;
    import Java.VM.Class.File.ConstantPool.Writer;
    import Java.VM.Class.File.CodeAttribute.Opcodes;
    import Java.VM.Class.File.IO.AttributeWriter;
    import Data.Bits;
    import Data.Char;
    
    --convert CodeAttribute to Attribute, given the index of the "Code" utf8 name
    writeCodeAttribute :: CodeAttribute -> CPoolIndex {-"Code" utf8 index-} -> Attribute;
    writeCodeAttribute (CodeAttribute maxStack maxLocals bytecode exceptionTable attributes) idx = 
        let code = writeByteCode bytecode in
            let codeData = writeOutputValues [
                               UI16 maxStack,
                               UI16 maxLocals,
                               UI32 $ i2i $ sizeOfOutputValue code,
                               code,
                               writeItems exceptionTable writeExceptionTableEntry,
                               writeItems attributes     writeAttribute 
                           ]
            in Attribute idx (i2i $ length codeData) (stringToSource codeData);

    writeByteCode :: [Instruction] -> OutputValue;
    writeByteCode bc = writeInstructions bc 0;
                 
    writeInstructions :: [Instruction] -> InstructionAddress -> OutputValue;
    writeInstructions [] _ = OutputNothing;
    writeInstructions (i:is) addr = let outval = writeInstruction i addr
                                    in OutputValues [outval, writeInstructions is (addr + (i2i $ sizeOfOutputValue outval))];
                 
    writeInstruction :: Instruction -> InstructionAddress -> OutputValue;
    writeInstruction i addr = case i of {
        (NOP                      ) -> UI8 op_NOP;
        (ACONST_NULL              ) -> UI8 op_ACONST_NULL;
        (ICONST_M1                ) -> UI8 op_ICONST_M1  ;  
        (ICONST_0                 ) -> UI8 op_ICONST_0   ;  
        (ICONST_1                 ) -> UI8 op_ICONST_1   ;  
        (ICONST_2                 ) -> UI8 op_ICONST_2   ;     
        (ICONST_3                 ) -> UI8 op_ICONST_3   ;     
        (ICONST_4                 ) -> UI8 op_ICONST_4   ;     
        (ICONST_5                 ) -> UI8 op_ICONST_5   ;     
        (LCONST_0                 ) -> UI8 op_LCONST_0   ;
        (LCONST_1                 ) -> UI8 op_LCONST_1   ;     
        (FCONST_0                 ) -> UI8 op_FCONST_0   ;     
        (FCONST_1                 ) -> UI8 op_FCONST_1   ;     
        (FCONST_2                 ) -> UI8 op_FCONST_2   ;     
        (DCONST_0                 ) -> UI8 op_DCONST_0   ;     
        (DCONST_1                 ) -> UI8 op_DCONST_1   ;     
        (BIPUSH    val            ) -> OutputValues [ UI8 op_BIPUSH, SI8  val ];
        (SIPUSH    val            ) -> OutputValues [ UI8 op_SIPUSH, SI16 val ];
        (LDC       idx            ) -> OutputValues [ UI8 op_LDC   , writeCPoolIndex8 idx ];
        (LDC_W     idx            ) -> OutputValues [ UI8 op_LDC_W , writeCPoolIndex idx ];
        (LDC2_W    idx            ) -> OutputValues [ UI8 op_LDC2_W, writeCPoolIndex idx ];
        (ILOAD     var            ) -> OutputValues [ UI8 op_ILOAD , UI8 $ i2i var ];
        (LLOAD     var            ) -> OutputValues [ UI8 op_LLOAD , UI8 $ i2i var ];
        (FLOAD     var            ) -> OutputValues [ UI8 op_FLOAD , UI8 $ i2i var ];
        (DLOAD     var            ) -> OutputValues [ UI8 op_DLOAD , UI8 $ i2i var ];
        (ALOAD     var            ) -> OutputValues [ UI8 op_ALOAD , UI8 $ i2i var ];
        (ILOAD_0                  ) -> UI8 op_ILOAD_0;    
        (ILOAD_1                  ) -> UI8 op_ILOAD_1;    
        (ILOAD_2                  ) -> UI8 op_ILOAD_2;    
        (ILOAD_3                  ) -> UI8 op_ILOAD_3;    
        (LLOAD_0                  ) -> UI8 op_LLOAD_0;    
        (LLOAD_1                  ) -> UI8 op_LLOAD_1;    
        (LLOAD_2                  ) -> UI8 op_LLOAD_2;    
        (LLOAD_3                  ) -> UI8 op_LLOAD_3;    
        (FLOAD_0                  ) -> UI8 op_FLOAD_0;    
        (FLOAD_1                  ) -> UI8 op_FLOAD_1;    
        (FLOAD_2                  ) -> UI8 op_FLOAD_2;    
        (FLOAD_3                  ) -> UI8 op_FLOAD_3;    
        (DLOAD_0                  ) -> UI8 op_DLOAD_0;    
        (DLOAD_1                  ) -> UI8 op_DLOAD_1;    
        (DLOAD_2                  ) -> UI8 op_DLOAD_2;    
        (DLOAD_3                  ) -> UI8 op_DLOAD_3;    
        (ALOAD_0                  ) -> UI8 op_ALOAD_0;    
        (ALOAD_1                  ) -> UI8 op_ALOAD_1;    
        (ALOAD_2                  ) -> UI8 op_ALOAD_2;    
        (ALOAD_3                  ) -> UI8 op_ALOAD_3;    
        (IALOAD                   ) -> UI8 op_IALOAD ;     
        (LALOAD                   ) -> UI8 op_LALOAD ;     
        (FALOAD                   ) -> UI8 op_FALOAD ;     
        (DALOAD                   ) -> UI8 op_DALOAD ;     
        (AALOAD                   ) -> UI8 op_AALOAD ;     
        (BALOAD                   ) -> UI8 op_BALOAD ;     
        (CALOAD                   ) -> UI8 op_CALOAD ;     
        (SALOAD                   ) -> UI8 op_SALOAD ;     
        (ISTORE    var            ) -> OutputValues [ UI8 op_ISTORE, UI8 $ i2i var ];
        (LSTORE    var            ) -> OutputValues [ UI8 op_LSTORE, UI8 $ i2i var ];
        (FSTORE    var            ) -> OutputValues [ UI8 op_FSTORE, UI8 $ i2i var ];
        (DSTORE    var            ) -> OutputValues [ UI8 op_DSTORE, UI8 $ i2i var ];
        (ASTORE    var            ) -> OutputValues [ UI8 op_ASTORE, UI8 $ i2i var ];
        (ISTORE_0                 ) -> UI8 op_ISTORE_0;   
        (ISTORE_1                 ) -> UI8 op_ISTORE_1;   
        (ISTORE_2                 ) -> UI8 op_ISTORE_2;   
        (ISTORE_3                 ) -> UI8 op_ISTORE_3;   
        (LSTORE_0                 ) -> UI8 op_LSTORE_0;   
        (LSTORE_1                 ) -> UI8 op_LSTORE_1;   
        (LSTORE_2                 ) -> UI8 op_LSTORE_2;   
        (LSTORE_3                 ) -> UI8 op_LSTORE_3;   
        (FSTORE_0                 ) -> UI8 op_FSTORE_0;   
        (FSTORE_1                 ) -> UI8 op_FSTORE_1;   
        (FSTORE_2                 ) -> UI8 op_FSTORE_2;   
        (FSTORE_3                 ) -> UI8 op_FSTORE_3;   
        (DSTORE_0                 ) -> UI8 op_DSTORE_0;   
        (DSTORE_1                 ) -> UI8 op_DSTORE_1;   
        (DSTORE_2                 ) -> UI8 op_DSTORE_2;   
        (DSTORE_3                 ) -> UI8 op_DSTORE_3;   
        (ASTORE_0                 ) -> UI8 op_ASTORE_0;   
        (ASTORE_1                 ) -> UI8 op_ASTORE_1;   
        (ASTORE_2                 ) -> UI8 op_ASTORE_2;   
        (ASTORE_3                 ) -> UI8 op_ASTORE_3;   
        (IASTORE                  ) -> UI8 op_IASTORE;    
        (LASTORE                  ) -> UI8 op_LASTORE;    
        (FASTORE                  ) -> UI8 op_FASTORE;    
        (DASTORE                  ) -> UI8 op_DASTORE;    
        (AASTORE                  ) -> UI8 op_AASTORE;    
        (BASTORE                  ) -> UI8 op_BASTORE;    
        (CASTORE                  ) -> UI8 op_CASTORE;    
        (SASTORE                  ) -> UI8 op_SASTORE;    
        (POP                      ) -> UI8 op_POP    ;        
        (POP2                     ) -> UI8 op_POP2   ;       
        (DUP                      ) -> UI8 op_DUP    ;        
        (DUP_X1                   ) -> UI8 op_DUP_X1 ;     
        (DUP_X2                   ) -> UI8 op_DUP_X2 ;     
        (DUP2                     ) -> UI8 op_DUP2   ;       
        (DUP2_X1                  ) -> UI8 op_DUP2_X1;    
        (DUP2_X2                  ) -> UI8 op_DUP2_X2;    
        (SWAP                     ) -> UI8 op_SWAP   ;       
        (IADD                     ) -> UI8 op_IADD   ;       
        (LADD                     ) -> UI8 op_LADD   ;       
        (FADD                     ) -> UI8 op_FADD   ;       
        (DADD                     ) -> UI8 op_DADD   ;       
        (ISUB                     ) -> UI8 op_ISUB   ;       
        (LSUB                     ) -> UI8 op_LSUB   ;       
        (FSUB                     ) -> UI8 op_FSUB   ;       
        (DSUB                     ) -> UI8 op_DSUB   ;       
        (IMUL                     ) -> UI8 op_IMUL   ;       
        (LMUL                     ) -> UI8 op_LMUL   ;       
        (FMUL                     ) -> UI8 op_FMUL   ;       
        (DMUL                     ) -> UI8 op_DMUL   ;       
        (IDIV                     ) -> UI8 op_IDIV   ;       
        (LDIV                     ) -> UI8 op_LDIV   ;       
        (FDIV                     ) -> UI8 op_FDIV   ;       
        (DDIV                     ) -> UI8 op_DDIV   ;       
        (IREM                     ) -> UI8 op_IREM   ;       
        (LREM                     ) -> UI8 op_LREM   ;       
        (FREM                     ) -> UI8 op_FREM   ;       
        (DREM                     ) -> UI8 op_DREM   ;       
        (INEG                     ) -> UI8 op_INEG   ;       
        (LNEG                     ) -> UI8 op_LNEG   ;       
        (FNEG                     ) -> UI8 op_FNEG   ;       
        (DNEG                     ) -> UI8 op_DNEG   ;       
        (ISHL                     ) -> UI8 op_ISHL   ;       
        (LSHL                     ) -> UI8 op_LSHL   ;       
        (ISHR                     ) -> UI8 op_ISHR   ;       
        (LSHR                     ) -> UI8 op_LSHR   ;       
        (IUSHR                    ) -> UI8 op_IUSHR  ;      
        (LUSHR                    ) -> UI8 op_LUSHR  ;      
        (IAND                     ) -> UI8 op_IAND   ;       
        (LAND                     ) -> UI8 op_LAND   ;       
        (IOR                      ) -> UI8 op_IOR    ;        
        (LOR                      ) -> UI8 op_LOR    ;        
        (IXOR                     ) -> UI8 op_IXOR   ;       
        (LXOR                     ) -> UI8 op_LXOR   ;       
        (IINC      var inc        ) -> OutputValues [ UI8 op_IINC, UI8 $ i2i var, SI8 $ i2i inc ];
        (I2L                      ) -> UI8 op_I2L  ;        
        (I2F                      ) -> UI8 op_I2F  ;        
        (I2D                      ) -> UI8 op_I2D  ;        
        (L2I                      ) -> UI8 op_L2I  ;        
        (L2F                      ) -> UI8 op_L2F  ;        
        (L2D                      ) -> UI8 op_L2D  ;        
        (F2I                      ) -> UI8 op_F2I  ;        
        (F2L                      ) -> UI8 op_F2L  ;        
        (F2D                      ) -> UI8 op_F2D  ;        
        (D2I                      ) -> UI8 op_D2I  ;        
        (D2L                      ) -> UI8 op_D2L  ;        
        (D2F                      ) -> UI8 op_D2F  ;        
        (I2B                      ) -> UI8 op_I2B  ;        
        (I2C                      ) -> UI8 op_I2C  ;        
        (I2S                      ) -> UI8 op_I2S  ;        
        (LCMP                     ) -> UI8 op_LCMP ;       
        (FCMPL                    ) -> UI8 op_FCMPL;      
        (FCMPG                    ) -> UI8 op_FCMPG;      
        (DCMPL                    ) -> UI8 op_DCMPL;      
        (DCMPG                    ) -> UI8 op_DCMPG;      
        (IFEQ      a              ) -> OutputValues [ UI8 op_IFEQ     , SI16 a ];
        (IFNE      a              ) -> OutputValues [ UI8 op_IFNE     , SI16 a ];
        (IFLT      a              ) -> OutputValues [ UI8 op_IFLT     , SI16 a ]; 
        (IFGE      a              ) -> OutputValues [ UI8 op_IFGE     , SI16 a ]; 
        (IFGT      a              ) -> OutputValues [ UI8 op_IFGT     , SI16 a ]; 
        (IFLE      a              ) -> OutputValues [ UI8 op_IFLE     , SI16 a ]; 
        (IF_ICMPEQ a              ) -> OutputValues [ UI8 op_IF_ICMPEQ, SI16 a ];
        (IF_ICMPNE a              ) -> OutputValues [ UI8 op_IF_ICMPNE, SI16 a ];
        (IF_ICMPLT a              ) -> OutputValues [ UI8 op_IF_ICMPLT, SI16 a ];
        (IF_ICMPGE a              ) -> OutputValues [ UI8 op_IF_ICMPGE, SI16 a ];      
        (IF_ICMPGT a              ) -> OutputValues [ UI8 op_IF_ICMPGT, SI16 a ];
        (IF_ICMPLE a              ) -> OutputValues [ UI8 op_IF_ICMPLE, SI16 a ];
        (IF_ACMPEQ a              ) -> OutputValues [ UI8 op_IF_ACMPEQ, SI16 a ];
        (IF_ACMPNE a              ) -> OutputValues [ UI8 op_IF_ACMPNE, SI16 a ];
        (GOTO      a              ) -> OutputValues [ UI8 op_GOTO     , SI16 a ];
        (JSR       a              ) -> OutputValues [ UI8 op_JSR      , SI16 a ];
        (RET       var            ) -> OutputValues [ UI8 op_RET      , UI8 $ i2i var ];
        (TABLESWITCH  _ _ _ _ _   ) -> writeTableSwitch  i;
        (LOOKUPSWITCH _ _ _ _     ) -> writeLookupSwitch i;
        (IRETURN                  ) -> UI8 op_IRETURN;
        (LRETURN                  ) -> UI8 op_LRETURN;
        (FRETURN                  ) -> UI8 op_FRETURN;
        (DRETURN                  ) -> UI8 op_DRETURN;
        (ARETURN                  ) -> UI8 op_ARETURN;
        (RETURN                   ) -> UI8 op_RETURN ;
        (GETSTATIC       idx      ) -> OutputValues [ UI8 op_GETSTATIC      , writeCPoolIndex idx ];
        (PUTSTATIC       idx      ) -> OutputValues [ UI8 op_PUTSTATIC      , writeCPoolIndex idx ];
        (GETFIELD        idx      ) -> OutputValues [ UI8 op_GETFIELD       , writeCPoolIndex idx ];
        (PUTFIELD        idx      ) -> OutputValues [ UI8 op_PUTFIELD       , writeCPoolIndex idx ];
        (INVOKEVIRTUAL   idx      ) -> OutputValues [ UI8 op_INVOKEVIRTUAL  , writeCPoolIndex idx ];
        (INVOKESPECIAL   idx      ) -> OutputValues [ UI8 op_INVOKESPECIAL  , writeCPoolIndex idx ];
        (INVOKESTATIC    idx      ) -> OutputValues [ UI8 op_INVOKESTATIC   , writeCPoolIndex idx ];
        (INVOKEINTERFACE idx ac   ) -> OutputValues [ UI8 op_INVOKEINTERFACE, writeCPoolIndex idx, UI8 ac, UI8 0 ];
        (NEW             idx      ) -> OutputValues [ UI8 op_NEW            , writeCPoolIndex idx ];
        (NEWARRAY        atype    ) -> OutputValues [ UI8 op_NEWARRAY       , writePrimArrayType atype ];
        (ANEWARRAY       idx      ) -> OutputValues [ UI8 op_ANEWARRAY      , writeCPoolIndex idx ];
        (ARRAYLENGTH              ) -> UI8 op_ARRAYLENGTH;
        (ATHROW                   ) -> UI8 op_ATHROW;
        (CHECKCAST       idx      ) -> OutputValues [ UI8 op_CHECKCAST , writeCPoolIndex idx ];
        (INSTANCEOF      idx      ) -> OutputValues [ UI8 op_INSTANCEOF, writeCPoolIndex idx ];
        (MONITORENTER             ) -> UI8 op_MONITORENTER;
        (MONITOREXIT              ) -> UI8 op_MONITOREXIT ;
        (WIDE            i        ) -> writeWideInstruction i;
        (MULTIANEWARRAY  idx dims ) -> OutputValues [ UI8 op_MULTIANEWARRAY, writeCPoolIndex idx, UI8 dims ];
        (IFNULL          a        ) -> OutputValues [ UI8 op_IFNULL        , SI16 a ];
        (IFNONNULL       a        ) -> OutputValues [ UI8 op_IFNONNULL     , SI16 a ];
        (GOTO_W          a        ) -> OutputValues [ UI8 op_GOTO_W        , SI32 a ];
        (JSR_W           a        ) -> OutputValues [ UI8 op_JSR_W         , SI32 a ];
    
    };
    
    writeTableSwitch :: Instruction -> OutputValue;
    writeTableSwitch (TABLESWITCH p a low hi aa) = OutputValues [
        UI8 op_TABLESWITCH,
        writeSwitchPadding p,
        SI32 a,
        SI32 low,
        SI32 hi,
        OutputValues (map (\offset->SI32 offset) aa)];
            
    writeLookupSwitch :: Instruction -> OutputValue;
    writeLookupSwitch (LOOKUPSWITCH p a n cases) = OutputValues [ 
        UI8 op_LOOKUPSWITCH,
        writeSwitchPadding p,
        SI32 a,
        UI32 n,
        OutputValues $ map (\(val,offset)->OutputValues [SI32 val, SI32 offset]) cases ];
        
    --write the padding after a switch opcode
    writeSwitchPadding :: Int -> OutputValue;
    writeSwitchPadding size = case size of {
                                  0 -> OutputNothing;
                                  1 -> UI8 0;
                                  2 -> OutputValues [UI8 0, UI8 0];
                                  3 -> OutputValues [UI8 0, UI8 0, UI8 0];
                              };
    
    writeExceptionTableEntry :: ExceptionTableEntry -> OutputValue;
    writeExceptionTableEntry (ExceptionTableEntry startPC endPC handlerPC catchTypeIndex) = 
        OutputValues [UI16 startPC, UI16 endPC, UI16 handlerPC, writeCPoolIndex catchTypeIndex ];
        
    writePrimArrayType :: PrimitiveArrayType -> OutputValue;
    writePrimArrayType t = case t of {
                               BooleanArray -> UI8 4 ;
                               CharArray    -> UI8 5 ;
                               FloatArray   -> UI8 6 ;
                               DoubleArray  -> UI8 7 ;
                               ByteArray    -> UI8 8 ;
                               ShortArray   -> UI8 9 ;
                               IntArray     -> UI8 10;
                               LongArray    -> UI8 11;
                           };
                           
    writeWideInstruction :: Instruction -> OutputValue;              
    writeWideInstruction i = OutputValues ((UI8 op_WIDE) : case i of {
        (ILOAD  var    ) -> [ UI8 op_ILOAD , UI16 var ];
        (LLOAD  var    ) -> [ UI8 op_LLOAD , UI16 var ];
        (FLOAD  var    ) -> [ UI8 op_FLOAD , UI16 var ];
        (DLOAD  var    ) -> [ UI8 op_DLOAD , UI16 var ];
        (ALOAD  var    ) -> [ UI8 op_ALOAD , UI16 var ];
        (ISTORE var    ) -> [ UI8 op_ISTORE, UI16 var ];
        (LSTORE var    ) -> [ UI8 op_LSTORE, UI16 var ];
        (FSTORE var    ) -> [ UI8 op_FSTORE, UI16 var ];
        (DSTORE var    ) -> [ UI8 op_DSTORE, UI16 var ];
        (ASTORE var    ) -> [ UI8 op_ASTORE, UI16 var ];
        (IINC   var inc) -> [ UI8 op_IINC  , UI16 var, SI16 inc ];
        (RET    var    ) -> [ UI8 op_RET   , UI16 var ];     
    });
}