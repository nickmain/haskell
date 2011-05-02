-- | JVM Instruction Opcode constants
module JVM.ClassFile.IO.Opcodes where { 

    import Data.Word(Word8);

    op_NOP             = 0x00 ::Word8;             
    op_ACONST_NULL     = 0x01 ::Word8;     
    op_ICONST_M1       = 0x02 ::Word8;       
    op_ICONST_0        = 0x03 ::Word8;        
    op_ICONST_1        = 0x04 ::Word8;        
    op_ICONST_2        = 0x05 ::Word8;        
    op_ICONST_3        = 0x06 ::Word8;
    op_ICONST_4        = 0x07 ::Word8;        
    op_ICONST_5        = 0x08 ::Word8;        
    op_LCONST_0        = 0x09 ::Word8;        
    op_LCONST_1        = 0x0a ::Word8;        
    op_FCONST_0        = 0x0b ::Word8;        
    op_FCONST_1        = 0x0c ::Word8;        
    op_FCONST_2        = 0x0d ::Word8;        
    op_DCONST_0        = 0x0e ::Word8;        
    op_DCONST_1        = 0x0f ::Word8;        
    op_BIPUSH          = 0x10 ::Word8;          
    op_SIPUSH          = 0x11 ::Word8;          
    op_LDC             = 0x12 ::Word8;             
    op_LDC_W           = 0x13 ::Word8;           
    op_LDC2_W          = 0x14 ::Word8;          
    op_ILOAD           = 0x15 ::Word8;           
    op_LLOAD           = 0x16 ::Word8;           
    op_FLOAD           = 0x17 ::Word8;           
    op_DLOAD           = 0x18 ::Word8;           
    op_ALOAD           = 0x19 ::Word8;           
    op_ILOAD_0         = 0x1a ::Word8;         
    op_ILOAD_1         = 0x1b ::Word8;         
    op_ILOAD_2         = 0x1c ::Word8;         
    op_ILOAD_3         = 0x1d ::Word8;         
    op_LLOAD_0         = 0x1e ::Word8;         
    op_LLOAD_1         = 0x1f ::Word8;         
    op_LLOAD_2         = 0x20 ::Word8;         
    op_LLOAD_3         = 0x21 ::Word8;         
    op_FLOAD_0         = 0x22 ::Word8;         
    op_FLOAD_1         = 0x23 ::Word8;         
    op_FLOAD_2         = 0x24 ::Word8;         
    op_FLOAD_3         = 0x25 ::Word8;         
    op_DLOAD_0         = 0x26 ::Word8;         
    op_DLOAD_1         = 0x27 ::Word8;         
    op_DLOAD_2         = 0x28 ::Word8;         
    op_DLOAD_3         = 0x29 ::Word8;         
    op_ALOAD_0         = 0x2a ::Word8;         
    op_ALOAD_1         = 0x2b ::Word8;         
    op_ALOAD_2         = 0x2c ::Word8;         
    op_ALOAD_3         = 0x2d ::Word8;         
    op_IALOAD          = 0x2e ::Word8;          
    op_LALOAD          = 0x2f ::Word8;          
    op_FALOAD          = 0x30 ::Word8;          
    op_DALOAD          = 0x31 ::Word8;          
    op_AALOAD          = 0x32 ::Word8;          
    op_BALOAD          = 0x33 ::Word8;          
    op_CALOAD          = 0x34 ::Word8;          
    op_SALOAD          = 0x35 ::Word8;          
    op_ISTORE          = 0x36 ::Word8;          
    op_LSTORE          = 0x37 ::Word8;          
    op_FSTORE          = 0x38 ::Word8;          
    op_DSTORE          = 0x39 ::Word8;          
    op_ASTORE          = 0x3a ::Word8;          
    op_ISTORE_0        = 0x3b ::Word8;        
    op_ISTORE_1        = 0x3c ::Word8;        
    op_ISTORE_2        = 0x3d ::Word8;        
    op_ISTORE_3        = 0x3e ::Word8;        
    op_LSTORE_0        = 0x3f ::Word8;        
    op_LSTORE_1        = 0x40 ::Word8;        
    op_LSTORE_2        = 0x41 ::Word8;        
    op_LSTORE_3        = 0x42 ::Word8;        
    op_FSTORE_0        = 0x43 ::Word8;        
    op_FSTORE_1        = 0x44 ::Word8;        
    op_FSTORE_2        = 0x45 ::Word8;        
    op_FSTORE_3        = 0x46 ::Word8;        
    op_DSTORE_0        = 0x47 ::Word8;        
    op_DSTORE_1        = 0x48 ::Word8;        
    op_DSTORE_2        = 0x49 ::Word8;        
    op_DSTORE_3        = 0x4a ::Word8;        
    op_ASTORE_0        = 0x4b ::Word8;        
    op_ASTORE_1        = 0x4c ::Word8;        
    op_ASTORE_2        = 0x4d ::Word8;        
    op_ASTORE_3        = 0x4e ::Word8;        
    op_IASTORE         = 0x4f ::Word8;         
    op_LASTORE         = 0x50 ::Word8;         
    op_FASTORE         = 0x51 ::Word8;         
    op_DASTORE         = 0x52 ::Word8;         
    op_AASTORE         = 0x53 ::Word8;         
    op_BASTORE         = 0x54 ::Word8;         
    op_CASTORE         = 0x55 ::Word8;         
    op_SASTORE         = 0x56 ::Word8;         
    op_POP             = 0x57 ::Word8;             
    op_POP2            = 0x58 ::Word8;            
    op_DUP             = 0x59 ::Word8;             
    op_DUP_X1          = 0x5a ::Word8;          
    op_DUP_X2          = 0x5b ::Word8;          
    op_DUP2            = 0x5c ::Word8;            
    op_DUP2_X1         = 0x5d ::Word8;         
    op_DUP2_X2         = 0x5e ::Word8;         
    op_SWAP            = 0x5f ::Word8;            
    op_IADD            = 0x60 ::Word8;            
    op_LADD            = 0x61 ::Word8;            
    op_FADD            = 0x62 ::Word8;            
    op_DADD            = 0x63 ::Word8;            
    op_ISUB            = 0x64 ::Word8;            
    op_LSUB            = 0x65 ::Word8;            
    op_FSUB            = 0x66 ::Word8;            
    op_DSUB            = 0x67 ::Word8;            
    op_IMUL            = 0x68 ::Word8;            
    op_LMUL            = 0x69 ::Word8;            
    op_FMUL            = 0x6a ::Word8;            
    op_DMUL            = 0x6b ::Word8;            
    op_IDIV            = 0x6c ::Word8;            
    op_LDIV            = 0x6d ::Word8;            
    op_FDIV            = 0x6e ::Word8;            
    op_DDIV            = 0x6f ::Word8;            
    op_IREM            = 0x70 ::Word8;            
    op_LREM            = 0x71 ::Word8;            
    op_FREM            = 0x72 ::Word8;            
    op_DREM            = 0x73 ::Word8;            
    op_INEG            = 0x74 ::Word8;            
    op_LNEG            = 0x75 ::Word8;            
    op_FNEG            = 0x76 ::Word8;            
    op_DNEG            = 0x77 ::Word8;            
    op_ISHL            = 0x78 ::Word8;            
    op_LSHL            = 0x79 ::Word8;            
    op_ISHR            = 0x7a ::Word8;            
    op_LSHR            = 0x7b ::Word8;            
    op_IUSHR           = 0x7c ::Word8;           
    op_LUSHR           = 0x7d ::Word8;           
    op_IAND            = 0x7e ::Word8;            
    op_LAND            = 0x7f ::Word8;            
    op_IOR             = 0x80 ::Word8;             
    op_LOR             = 0x81 ::Word8;             
    op_IXOR            = 0x82 ::Word8;            
    op_LXOR            = 0x83 ::Word8;            
    op_IINC            = 0x84 ::Word8;            
    op_I2L             = 0x85 ::Word8;             
    op_I2F             = 0x86 ::Word8;             
    op_I2D             = 0x87 ::Word8;             
    op_L2I             = 0x88 ::Word8;             
    op_L2F             = 0x89 ::Word8;             
    op_L2D             = 0x8a ::Word8;             
    op_F2I             = 0x8b ::Word8;             
    op_F2L             = 0x8c ::Word8;             
    op_F2D             = 0x8d ::Word8;             
    op_D2I             = 0x8e ::Word8;             
    op_D2L             = 0x8f ::Word8;             
    op_D2F             = 0x90 ::Word8;             
    op_I2B             = 0x91 ::Word8;             
    op_I2C             = 0x92 ::Word8;             
    op_I2S             = 0x93 ::Word8;             
    op_LCMP            = 0x94 ::Word8;            
    op_FCMPL           = 0x95 ::Word8;           
    op_FCMPG           = 0x96 ::Word8;           
    op_DCMPL           = 0x97 ::Word8;           
    op_DCMPG           = 0x98 ::Word8;           
    op_IFEQ            = 0x99 ::Word8;            
    op_IFNE            = 0x9a ::Word8;            
    op_IFLT            = 0x9b ::Word8;            
    op_IFGE            = 0x9c ::Word8;            
    op_IFGT            = 0x9d ::Word8;            
    op_IFLE            = 0x9e ::Word8;            
    op_IF_ICMPEQ       = 0x9f ::Word8;       
    op_IF_ICMPNE       = 0xa0 ::Word8;       
    op_IF_ICMPLT       = 0xa1 ::Word8;       
    op_IF_ICMPGE       = 0xa2 ::Word8;       
    op_IF_ICMPGT       = 0xa3 ::Word8;       
    op_IF_ICMPLE       = 0xa4 ::Word8;       
    op_IF_ACMPEQ       = 0xa5 ::Word8;       
    op_IF_ACMPNE       = 0xa6 ::Word8;       
    op_GOTO            = 0xa7 ::Word8;            
    op_JSR             = 0xa8 ::Word8;             
    op_RET             = 0xa9 ::Word8;             
    op_TABLESWITCH     = 0xaa ::Word8;     
    op_LOOKUPSWITCH    = 0xab ::Word8;    
    op_IRETURN         = 0xac ::Word8;         
    op_LRETURN         = 0xad ::Word8;         
    op_FRETURN         = 0xae ::Word8;         
    op_DRETURN         = 0xaf ::Word8;         
    op_ARETURN         = 0xb0 ::Word8;         
    op_RETURN          = 0xb1 ::Word8;          
    op_GETSTATIC       = 0xb2 ::Word8;       
    op_PUTSTATIC       = 0xb3 ::Word8;       
    op_GETFIELD        = 0xb4 ::Word8;        
    op_PUTFIELD        = 0xb5 ::Word8;        
    op_INVOKEVIRTUAL   = 0xb6 ::Word8;   
    op_INVOKESPECIAL   = 0xb7 ::Word8;   
    op_INVOKESTATIC    = 0xb8 ::Word8;    
    op_INVOKEINTERFACE = 0xb9 ::Word8;                          
    op_NEW             = 0xbb ::Word8;             
    op_NEWARRAY        = 0xbc ::Word8;        
    op_ANEWARRAY       = 0xbd ::Word8;       
    op_ARRAYLENGTH     = 0xbe ::Word8;     
    op_ATHROW          = 0xbf ::Word8;          
    op_CHECKCAST       = 0xc0 ::Word8;       
    op_INSTANCEOF      = 0xc1 ::Word8;      
    op_MONITORENTER    = 0xc2 ::Word8;    
    op_MONITOREXIT     = 0xc3 ::Word8;     
    op_WIDE            = 0xc4 ::Word8;            
    op_MULTIANEWARRAY  = 0xc5 ::Word8;  
    op_IFNULL          = 0xc6 ::Word8;          
    op_IFNONNULL       = 0xc7 ::Word8;       
    op_GOTO_W          = 0xc8 ::Word8;          
    op_JSR_W           = 0xc9 ::Word8;           
    op_BREAKPOINT      = 0xca ::Word8;      
    op_IMPDEP1         = 0xfe ::Word8;         
    op_IMPDEP2         = 0xff ::Word8;   
}