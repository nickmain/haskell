-- | Binary parsing utils
module Java.VM.Class.File.IO.ParserUtil where {

    import Data.Char;
    import Data.Bits;
    import Data.Int;
    import Data.Word;
    import System.IO;

    -- | The result of a parse
    data ParseResult a = ParseSuccess a 
                       | ParseFailure String --error message
        deriving (Show,Eq);

    -- | Binary source
    type ParseSource = [Word8];
    
    data ParseState a = Parsing    a ParseSource
                      | ParseError (ParseComputation a) String;

    data ParseComputation a = ParseComputation (ParseSource->ParseState a);

    --The parsing Monad     
    instance Monad ParseComputation where {
        -- f1 :: ParseSource -> ParseState a
        -- f2 :: a -> ParseComputation b
        ParseComputation f1 >>= f2 = ParseComputation (\source -> 
                                         case (f1 source) of {
                                             (ParseError a s) -> ParseError (a >>= f2) s;
                                             (Parsing    a s) -> let (ParseComputation b) = f2 a
                                                                 in b s;                                         
                                         });
    
        -- return :: a -> ParseComputation a
        return a = ParseComputation (\s->Parsing a s);

        -- fail   :: String -> ParseComputation a
        fail s = ParseComputation (\_->ParseError (fail s) s);
    };
    
    --Convert from one int type to another
    i2i :: (Integral a, Num b) => a -> b;
    i2i = fromIntegral; --fromInteger $ toInteger a;
    
    --Extract the current source from a computation
    getParseSource :: ParseComputation ParseSource;
    getParseSource =  ParseComputation (\s->Parsing s s);
    
    --Update the parse source
    updateParseSource :: ParseSource -> ParseComputation ();
    updateParseSource s = ParseComputation (\x->Parsing () s);
    
    --convert String to ParseSource
    stringToSource :: String -> ParseSource;
    stringToSource [] = [];
    stringToSource (c:cs) = (toEnum $ ord c) : (stringToSource cs);
    
    --Run a parse
    parse :: ParseSource -> ParseComputation a -> ParseResult a;
    parse source (ParseComputation f) = case (f source) of {
                                               (Parsing a s)     -> ParseSuccess a;
                                               (ParseError a s ) -> ParseFailure s;
                                           };      
        
    --Parse a file
    parseFile :: FilePath -> ParseComputation a -> IO (ParseResult a);
    parseFile filename comp = do {
                                    handle <- openBinaryFile filename ReadMode;
                                    source <- hGetContents handle;
                                    return $ parse (stringToSource source) comp;
                              };
        
    --Parse a given number of items
    parseItems :: (Integral i) => i {-count-} -> ParseComputation a -> ParseComputation [a];
    parseItems 0 _ = return [];
    parseItems count comp = do {
                                item  <- comp;
                                items <- parseItems (count-1) comp;
                                return (item:items);
                            };      
        
    --Parse a given number of items preceded by a UI16 count
    parseItemsUI16 :: ParseComputation a -> ParseComputation [a];
    parseItemsUI16 pc = do { 
        count <- readUI16; 
        parseItems count pc 
    };
        
    --return a given number of chars from the source
    readSourceElements :: (Integral a) => a -> ParseComputation ParseSource;
    readSourceElements size = parseItems size readUI8;  

    readChar :: ParseComputation Char;
    readChar = do {
        byte <- readUI8;
        return $ chr $ fromIntegral byte;
    };
        
    readUI8 :: ParseComputation Word8;
    readUI8 = ParseComputation (\cs -> case cs of {
                  (a:b)     -> Parsing a b;
                  otherwise -> ParseError (return 0) "Unexpected end of source when reading a UI8.";
              });
        
    readUI16 :: ParseComputation Word16;
    readUI16 = ParseComputation (\cs -> case cs of {
                   (a:b:c)   -> Parsing ((i2i a) * 256 + (i2i b)) c;
                   otherwise -> ParseError (return 0) "Unexpected end of source when reading a UI16.";
               });

    readUI32 :: ParseComputation Word32;
    readUI32 = ParseComputation (\cs -> case cs of {
                   (a:b:c:d:e) -> Parsing ((b2i a 3) + (b2i b 2) + (b2i c 1) + (b2i d 0)) e;
                   otherwise   -> ParseError (return 0) "Unexpected end of source when reading a UI32.";
               });

    readSI8 :: ParseComputation Int8;
    readSI8 = ParseComputation (\cs -> case cs of {
                  (a:b)     -> Parsing (signByte a) b;
                  otherwise -> ParseError (return 0) "Unexpected end of source when reading an SI8.";
              });

    readSI16 :: ParseComputation Int16;
    readSI16 = ParseComputation (\cs -> case cs of {
                   (a:b:c)   -> Parsing (signShort ((i2i a) * 256 + (i2i b))) c;
                   otherwise -> ParseError (return 0) "Unexpected end of source when reading an SI16.";
               });

    readSI32 :: ParseComputation Int32;
    readSI32 = ParseComputation (\cs -> case cs of {
                   (a:b:c:d:e) -> Parsing (signInteger ((b2i a 3) + (b2i b 2) + (b2i c 1) + (b2i d 0))) e;
                   otherwise   -> ParseError (return 0) "Unexpected end of source when reading an SI32.";
               });

    readSI64 :: ParseComputation Int64;
    readSI64 = ParseComputation (\cs -> case cs of {
                   (a:b:c:d:e:f:g:h:i) -> Parsing (signLong ((b2i a 7) + (b2i b 6) + (b2i c 5) + (b2i d 4) + (b2i e 3) + (b2i f 2) + (b2i g 1) + (b2i h 0))) i;
                   otherwise   -> ParseError (return 0) "Unexpected end of source when reading an SI64.";
               });


    readUI64 :: ParseComputation Word64;
    readUI64 = ParseComputation (\cs -> case cs of {
                   (a:b:c:d:e:f:g:h:i) -> Parsing ((b2i a 7) + (b2i b 6) + (b2i c 5) + (b2i d 4) + (b2i e 3) + (b2i f 2) + (b2i g 1) + (b2i h 0)) i;
                   otherwise   -> ParseError (return 0) "Unexpected end of source when reading a UI64.";
               });

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
    
    readFloat :: ParseComputation Float;
    readFloat = do {
                    bits <- readUI32;
                    return $ convert32Bits2Float bits;
                };
    
    readDouble :: ParseComputation Double;
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