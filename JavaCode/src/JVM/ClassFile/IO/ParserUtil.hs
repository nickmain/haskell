-- | Binary parsing utils
module JVM.ClassFile.IO.ParserUtil where {

    import Data.Char;
    import Data.Bits;
    import Data.Int;
    import Data.Word;
    import Control.Monad.State;
    import Control.Monad.Error;
    import Data.Either;
    import qualified Data.ByteString as BS;
    import JVM.ClassFile.Util.JVMOp;

     -- | Binary source
    type ParseSource = [Word8];
       
    -- | Class for state types that contain parse source
    class SourceContainer a where {
        getSrc :: JVMOp a ParseSource;
        putSrc :: ParseSource -> JVMOp a ();
        getBytesRead :: JVMOp a Int;        -- num bytes read
        incBytesRead :: Int -> JVMOp a ();  -- increment the bytes-read count
    };       
       
    --Convert from one int type to another
    i2i :: (Integral a, Num b) => a -> b;
    i2i = fromIntegral; --fromInteger $ toInteger a;
                        
    -- | Run a parse
    parse :: (SourceContainer a) => ParseSource -> a -> JVMOp a b -> Either JVMException b;
    parse src a op = let initSrc = do {
        putSrc src;
        op
    } in evalState (runErrorT initSrc) (JVMState a);
        
    --Parse a file
    parseFile :: (SourceContainer a) => FilePath -> a -> JVMOp a b -> IO (Either JVMException b);
    parseFile filename a comp = do {
                                    source <- BS.readFile filename;
                                    return $ parse (BS.unpack source) a comp;
                                };
    --Parse a given number of items
    parseItems :: (SourceContainer a, Integral i) => i {-count-} -> JVMOp a b -> JVMOp a [b];
    parseItems 0 _ = return [];
    parseItems count comp = do {
                                item  <- comp;
                                items <- parseItems (count-1) comp;
                                return (item:items);
                            };      
        
    --Parse a given number of items preceded by a UI16 count
    parseItemsUI16 :: (SourceContainer a) => JVMOp a b -> JVMOp a [b];
    parseItemsUI16 pc = do { 
        count <- readUI16; 
        parseItems count pc 
    };
        
    --return a given number of chars from the source
    readSourceElements :: (SourceContainer a, Integral c) => c -> JVMOp a ParseSource;
    readSourceElements size = parseItems size readUI8;  

    -- | Read a char
    readChar :: (SourceContainer a) => JVMOp a Char;
    readChar = do {
        byte <- readUI8;
        return $ chr $ fromIntegral byte;
    };
        
    -- | Read an unsigned byte
    readUI8 :: (SourceContainer a) => JVMOp a Word8;
    readUI8 = do {
        src <- getSrc;
        case src of {
            (b:bb)    -> do { incBytesRead 1; putSrc bb; return b };
            otherwise -> throwError "Unexpected end of data reading UI8"
        }
    };
    
    -- | Read an unsigned 16 bit word
    readUI16 :: (SourceContainer a) => JVMOp a Word16;
    readUI16 = do {
        src <- getSrc;
        case src of {
            (a:b:bb)  -> do { 
                             incBytesRead 2; 
                             putSrc bb; 
                             return $ fromIntegral ((a*256) + b) 
                         };
            otherwise -> throwError "Unexpected end of data reading UI16"
        }
    };

    -- | Read an unsigned 32 bit word
    readUI32 :: (SourceContainer a) => JVMOp a Word32;
    readUI32 = do {
        src <- getSrc;
        case src of {
            (a:b:c:d:bb)  -> do { 
                                 incBytesRead 4; 
                                 putSrc bb; 
                                 return $ fromIntegral((b2i a 3) + (b2i b 2) + (b2i c 1) + (b2i d 0)) 
                             };
            otherwise -> throwError "Unexpected end of data reading UI32"
        }    
    };

    -- | Read a signed byte
    readSI8 :: (SourceContainer a) => JVMOp a Int8;
    readSI8 = do {
        src <- getSrc;
        case src of {
            (b:bb)    -> do { incBytesRead 1; putSrc bb; return (signByte b) };
            otherwise -> throwError "Unexpected end of data reading SI8"
        }
    };

    readSI16 :: (SourceContainer a) => JVMOp a Int16;
    readSI16 = do {
        src <- getSrc;
        case src of {
            (a:b:bb)  -> do { 
                             incBytesRead 2; putSrc bb; 
                             return (signShort ((i2i a) * 256 + (i2i b))) 
                         };
            otherwise -> throwError "Unexpected end of data reading SI16"
        }
    };
    
    readSI32 :: (SourceContainer a) => JVMOp a Int32;
    readSI32 = do {
        src <- getSrc;
        case src of {
            (a:b:c:d:bb)  -> do { 
                                 incBytesRead 4; 
                                 putSrc bb; 
                                 return (signInteger ((b2i a 3) + (b2i b 2) + (b2i c 1) + (b2i d 0))) 
                             };
            otherwise -> throwError "Unexpected end of data reading SI32"
        }    
    };

    readSI64 :: (SourceContainer a) => JVMOp a Int64;
    readSI64 = do {
        src <- getSrc;
        case src of {
            (a:b:c:d:e:f:g:h:bb)  -> do {
                                         incBytesRead 8; 
                                         putSrc bb; 
                                         return (signLong ((b2i a 7) + (b2i b 6) + (b2i c 5) + (b2i d 4) + (b2i e 3) + (b2i f 2) + (b2i g 1) + (b2i h 0))) 
                                     };
            otherwise -> throwError "Unexpected end of data reading SI64"
        }    
    };
 
    readUI64 :: (SourceContainer a) => JVMOp a Word64;
    readUI64 = do {
        src <- getSrc;
        case src of {
            (a:b:c:d:e:f:g:h:bb)  -> do { 
                                         incBytesRead 8; 
                                         putSrc bb; 
                                         return (((b2i a 7) + (b2i b 6) + (b2i c 5) + (b2i d 4) + (b2i e 3) + (b2i f 2) + (b2i g 1) + (b2i h 0))) 
                                     };
            otherwise -> throwError "Unexpected end of data reading UI64"
        }    
    };

    b2i :: (Integral a, Num b) => a -> Integer {-power-} -> b;
    b2i b p = fromInteger $ (toInteger b) * (256 ^ p);
        
    signByte :: Word8 -> Int8;
    signByte i = if i >= 0x80 then i2i ((toInteger i) - 0x100) else i2i i;

    signShort :: Word16 -> Int16;
    signShort i = if i >= 0x8000 then i2i ((toInteger i) - 0x10000) else i2i i;
        
    signInteger :: Word32 -> Int32;
    signInteger i = if i >= 0x80000000 then i2i ((toInteger i) - 0x100000000) else i2i i;

    signLong :: Word64 -> Int64;
    signLong i = if i >= 0x8000000000000000 then i2i ((toInteger  i) - 0x10000000000000000) else i2i i;
    
    readFloat :: (SourceContainer a) => JVMOp a Float;
    readFloat = do {
                    bits <- readUI32;
                    return $ convert32Bits2Float bits;
                };
    
    readDouble :: (SourceContainer a) => JVMOp a Double;
    readDouble = do {
                     bits <- readUI64;
                     return $ convert64Bits2Double bits;
                 };
                 

    convert32Bits2Float :: Word32 -> Float;
    convert32Bits2Float 0x7f800000 =  1.0/0;  --positive infinity
    convert32Bits2Float 0xff800000 = -1.0/0;  --negative infinity
    convert32Bits2Float w | (w >= 0x7f800001) && (w <= 0x7fffffff) = 0/0 --NaN
                          | (w >= 0xff800001) && (w <= 0xffffffff) = 0/0 --NaN
                          | otherwise = let i = toInteger w in
                                         let sign = fromInteger (if (i .&. 0x80000000) == 0 then 1 else -1)
                                             expo = fromInteger ((i `shiftR` 23) .&. 0xff)
                                         in let mantissa = fromInteger (if expo == 0 then (i .&. 0x7fffff) `shiftL` 1
                                                                        else (i .&. 0x7fffff) .|. 0x800000)
                                            in sign * mantissa * (2.0 ^^ (expo-150));

    convert64Bits2Double :: Word64 -> Double;
    convert64Bits2Double 0x7ff0000000000000 =  1.0/0;  --positive infinity
    convert64Bits2Double 0xfff0000000000000 = -1.0/0;  --negative infinity
    convert64Bits2Double w | (w >= 0x7ff0000000000001) && (w <= 0x7fffffffffffffff) = 0/0 --NaN
                           | (w >= 0xfff0000000000001) && (w <= 0xffffffffffffffff) = 0/0 --NaN
                           | otherwise = let i = toInteger w in
                                         let sign = fromInteger (if (i .&. 0x8000000000000000) == 0 then 1 else -1)
                                             expo = fromInteger ((i `shiftR` 52) .&. 0x7ff)
                                         in let mantissa = fromInteger (if expo == 0 then (i .&. 0xfffffffffffff) `shiftL` 1
                                                                        else (i .&. 0xfffffffffffff) .|. 0x10000000000000)
                                            in sign * mantissa * (2.0 ^^ (expo-1075));
    
    --convert a char array of UTF8 encoded bytes to a unicode string
    decodeUTF8 :: ParseSource -> String;
    decodeUTF8 [] = [];
    decodeUTF8 (c:cs)     | c < 128 = (chr $ i2i c) : (decodeUTF8 cs); --normal ascii char
    decodeUTF8 (c:d:cs)   | (c .&. 0xe0) == 0xc0 = (chr $ (shiftChar c 0x1f 6 ) + (i2i (d .&. 0x3f))) : (decodeUTF8 cs);
    decodeUTF8 (c:d:e:cs) | (c .&. 0xf0) == 0xe0 = (chr $ (shiftChar c 0x0f 12) + (shiftChar d 0x3f 6) + (i2i (e .&. 0x3f))) : (decodeUTF8 cs);
    decodeUTF8 otherwise = " BAD UTF8 ENCODING";   
    
    shiftChar :: Word8 -> Word8 -> Int -> Int;
    shiftChar byte mask shft = (i2i (byte .&. mask)) `shiftL` shft;  

}