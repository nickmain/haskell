-- | Model of JVM Instructions
module Java.VM.Class.File.CodeAttribute (

    module Java.VM.Class.File.CodeAttribute,
    module Java.VM.Class.File.ConstantPool,
    module Data.Word,
    module Data.Int,
    Attribute
    
) where {

    import Java.VM.Class.File.Attribute;
    import Java.VM.Class.File.ConstantPool;
    import Data.Word;
    import Data.Int;

    --The JVM Code attribute
    data CodeAttribute = CodeAttribute { 
                             maxStack, maxLocals :: Word16, 
                             bytecode       :: [Instruction],
                             exceptionTable :: [ExceptionTableEntry],
                             codeAttributes :: [Attribute]
                         }
        deriving Eq;

    data ExceptionTableEntry = ExceptionTableEntry { 
        startPC, endPC, handlerPC :: InstructionAddress, 
        catchTypeIndex :: CPoolIndex 
    } deriving (Show,Eq);
    
    type LocalVar            = Word16;
    type InstructionAddress  = Word16;
    type InstructionOffset16 = Int16;
    type InstructionOffset32 = Int32;
    
    -- | JVM Instructions
    data Instruction = NOP 
                     | ACONST_NULL      
                     | ICONST_M1          
                     | ICONST_0            
                     | ICONST_1            
                     | ICONST_2            
                     | ICONST_3            
                     | ICONST_4            
                     | ICONST_5            
                     | LCONST_0            
                     | LCONST_1            
                     | FCONST_0            
                     | FCONST_1            
                     | FCONST_2            
                     | DCONST_0            
                     | DCONST_1            
                     | BIPUSH    Int8  {-value-}
                     | SIPUSH    Int16 {-value-}
                     | LDC       CPoolIndex
                     | LDC_W     CPoolIndex
                     | LDC2_W    CPoolIndex
                     | ILOAD     LocalVar
                     | LLOAD     LocalVar
                     | FLOAD     LocalVar
                     | DLOAD     LocalVar
                     | ALOAD     LocalVar
                     | ILOAD_0              
                     | ILOAD_1              
                     | ILOAD_2              
                     | ILOAD_3              
                     | LLOAD_0              
                     | LLOAD_1              
                     | LLOAD_2              
                     | LLOAD_3              
                     | FLOAD_0              
                     | FLOAD_1              
                     | FLOAD_2              
                     | FLOAD_3              
                     | DLOAD_0              
                     | DLOAD_1              
                     | DLOAD_2              
                     | DLOAD_3              
                     | ALOAD_0              
                     | ALOAD_1              
                     | ALOAD_2              
                     | ALOAD_3              
                     | IALOAD                
                     | LALOAD                
                     | FALOAD                
                     | DALOAD                
                     | AALOAD                
                     | BALOAD                
                     | CALOAD                
                     | SALOAD                
                     | ISTORE    LocalVar
                     | LSTORE    LocalVar
                     | FSTORE    LocalVar
                     | DSTORE    LocalVar
                     | ASTORE    LocalVar
                     | ISTORE_0            
                     | ISTORE_1            
                     | ISTORE_2            
                     | ISTORE_3            
                     | LSTORE_0            
                     | LSTORE_1            
                     | LSTORE_2            
                     | LSTORE_3            
                     | FSTORE_0            
                     | FSTORE_1            
                     | FSTORE_2            
                     | FSTORE_3            
                     | DSTORE_0            
                     | DSTORE_1            
                     | DSTORE_2            
                     | DSTORE_3            
                     | ASTORE_0            
                     | ASTORE_1            
                     | ASTORE_2            
                     | ASTORE_3            
                     | IASTORE              
                     | LASTORE              
                     | FASTORE              
                     | DASTORE              
                     | AASTORE              
                     | BASTORE              
                     | CASTORE              
                     | SASTORE              
                     | POP                      
                     | POP2                    
                     | DUP                      
                     | DUP_X1                
                     | DUP_X2                
                     | DUP2                    
                     | DUP2_X1              
                     | DUP2_X2              
                     | SWAP                    
                     | IADD                    
                     | LADD                    
                     | FADD                    
                     | DADD                    
                     | ISUB                    
                     | LSUB                    
                     | FSUB                    
                     | DSUB                    
                     | IMUL                    
                     | LMUL                    
                     | FMUL                    
                     | DMUL                    
                     | IDIV                    
                     | LDIV                    
                     | FDIV                    
                     | DDIV                    
                     | IREM                    
                     | LREM                    
                     | FREM                    
                     | DREM                    
                     | INEG                    
                     | LNEG                    
                     | FNEG                    
                     | DNEG                    
                     | ISHL                    
                     | LSHL                    
                     | ISHR                    
                     | LSHR                    
                     | IUSHR                  
                     | LUSHR                  
                     | IAND                    
                     | LAND                    
                     | IOR                      
                     | LOR                      
                     | IXOR                    
                     | LXOR                    
                     | IINC      LocalVar Int16 {-increment-}
                     | I2L                      
                     | I2F                      
                     | I2D                      
                     | L2I                      
                     | L2F                      
                     | L2D                      
                     | F2I                      
                     | F2L                      
                     | F2D                      
                     | D2I                      
                     | D2L                      
                     | D2F                      
                     | I2B                      
                     | I2C                      
                     | I2S                      
                     | LCMP                    
                     | FCMPL                  
                     | FCMPG                  
                     | DCMPL                  
                     | DCMPG                  
                     | IFEQ      InstructionOffset16       
                     | IFNE      InstructionOffset16       
                     | IFLT      InstructionOffset16       
                     | IFGE      InstructionOffset16       
                     | IFGT      InstructionOffset16       
                     | IFLE      InstructionOffset16       
                     | IF_ICMPEQ InstructionOffset16  
                     | IF_ICMPNE InstructionOffset16  
                     | IF_ICMPLT InstructionOffset16  
                     | IF_ICMPGE InstructionOffset16  
                     | IF_ICMPGT InstructionOffset16  
                     | IF_ICMPLE InstructionOffset16  
                     | IF_ACMPEQ InstructionOffset16  
                     | IF_ACMPNE InstructionOffset16  
                     | GOTO      InstructionOffset16       
                     | JSR       InstructionOffset16        
                     | RET       LocalVar        
                     | TABLESWITCH  Int {-padding size-}  InstructionOffset32 {-default-} Int32 {-low-} Int32 {-high-} [InstructionOffset32]
                     | LOOKUPSWITCH Int {-padding size-} InstructionOffset32 {-default-} Word32 {-num pairs-} [(Int32,InstructionOffset32)]
                     | IRETURN              
                     | LRETURN              
                     | FRETURN              
                     | DRETURN              
                     | ARETURN              
                     | RETURN                
                     | GETSTATIC       CPoolIndex {-Field Ref-}
                     | PUTSTATIC       CPoolIndex {-Field Ref-}
                     | GETFIELD        CPoolIndex {-Field Ref-}
                     | PUTFIELD        CPoolIndex {-Field Ref-}
                     | INVOKEVIRTUAL   CPoolIndex {-Method Ref-}
                     | INVOKESPECIAL   CPoolIndex {-Method Ref-}
                     | INVOKESTATIC    CPoolIndex {-Method Ref-}
                     | INVOKEINTERFACE CPoolIndex {-Method Ref-} Word8 {-arg count-}
                     | NEW             CPoolIndex {-Class Ref-}         
                     | NEWARRAY        PrimitiveArrayType    
                     | ANEWARRAY       CPoolIndex {-Class Ref-}    
                     | ARRAYLENGTH      
                     | ATHROW                
                     | CHECKCAST       CPoolIndex {-Class Ref-}    
                     | INSTANCEOF      CPoolIndex {-Class Ref-}   
                     | MONITORENTER    
                     | MONITOREXIT      
                     | WIDE            Instruction
                     | MULTIANEWARRAY  CPoolIndex {-Class Ref-} Word8 {-num dimensions-}
                     | IFNULL          InstructionOffset16         
                     | IFNONNULL       InstructionOffset16      
                     | GOTO_W          InstructionOffset32         
                     | JSR_W           InstructionOffset32
        deriving (Show,Eq);     
                    
    data PrimitiveArrayType = BooleanArray | CharArray | FloatArray | DoubleArray | ByteArray | ShortArray | IntArray | LongArray
        deriving (Show,Eq);
    
    -- | Get the bytesize of an instruction (given its address, since switch instructions depend on their alignment)
    sizeofInstruction :: (Integral a) => Instruction -> InstructionAddress -> a;
    sizeofInstruction i a = case i of {    
        (BIPUSH          _  ) -> 2;
        (SIPUSH          _  ) -> 3;
        (LDC             _  ) -> 2;
        (LDC_W           _  ) -> 3;
        (LDC2_W          _  ) -> 3;
        (ILOAD           _  ) -> 2;
        (LLOAD           _  ) -> 2;
        (FLOAD           _  ) -> 2;
        (DLOAD           _  ) -> 2;
        (ALOAD           _  ) -> 2;
        (ISTORE          _  ) -> 2;
        (LSTORE          _  ) -> 2;
        (FSTORE          _  ) -> 2;
        (DSTORE          _  ) -> 2;
        (ASTORE          _  ) -> 2;
        (IINC            _ _) -> 3;
        (IFEQ            _  ) -> 3;       
        (IFNE            _  ) -> 3;       
        (IFLT            _  ) -> 3;       
        (IFGE            _  ) -> 3;       
        (IFGT            _  ) -> 3;       
        (IFLE            _  ) -> 3;       
        (IF_ICMPEQ       _  ) -> 3;  
        (IF_ICMPNE       _  ) -> 3;  
        (IF_ICMPLT       _  ) -> 3;  
        (IF_ICMPGE       _  ) -> 3;  
        (IF_ICMPGT       _  ) -> 3;  
        (IF_ICMPLE       _  ) -> 3;  
        (IF_ACMPEQ       _  ) -> 3;  
        (IF_ACMPNE       _  ) -> 3;  
        (GOTO            _  ) -> 3;       
        (JSR             _  ) -> 3;        
        (RET             _  ) -> 2;
        (GETSTATIC       _  ) -> 3;
        (PUTSTATIC       _  ) -> 3;
        (GETFIELD        _  ) -> 3;
        (PUTFIELD        _  ) -> 3;
        (INVOKEVIRTUAL   _  ) -> 3;
        (INVOKESPECIAL   _  ) -> 3;
        (INVOKESTATIC    _  ) -> 3;
        (INVOKEINTERFACE _ _) -> 5;
        (NEW             _  ) -> 3;
        (NEWARRAY        _  ) -> 2;
        (ANEWARRAY       _  ) -> 3;
        (CHECKCAST       _  ) -> 3;
        (INSTANCEOF      _  ) -> 3;
        (WIDE            i2 ) -> 2 * (sizeofInstruction i2 a);
        (MULTIANEWARRAY  _ _) -> 4;
        (IFNULL          _  ) -> 3;
        (IFNONNULL       _  ) -> 3;
        (GOTO_W          _  ) -> 5;
        (JSR_W           _  ) -> 5;
        (TABLESWITCH p _ l h _) -> fromIntegral (13 + p + (4 * ((fromIntegral(h - l)) + 1)));
        (LOOKUPSWITCH p _ n _ ) -> fromIntegral (9 + p + (8 * (fromIntegral n)));
        otherwise -> 1;
    };
    
    instance Show CodeAttribute where {
        show (CodeAttribute maxStack maxLocals instructions exceptionTable attrs) =
             "\nCodeAttribute max" ++ (show (maxStack,maxLocals)) ++ "\n" ++
             (concat $ map (\ins->"   " ++ (show ins) ++ "\n") instructions) ++ 
             (show exceptionTable ) ++ "\n";
    };
    

}