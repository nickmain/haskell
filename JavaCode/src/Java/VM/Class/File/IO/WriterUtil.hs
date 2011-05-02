-- | Utils for writing binary data
module Java.VM.Class.File.IO.WriterUtil where {

    import Data.Bits;
    import Data.Char;
    import Data.Int;
    import Data.Word;

    {-
        Note: this is a slight mess - it would be better to work with Word8
        internally and only convert to String when writing to a file.
        This mess is due to incremental development with no forethought. :(    
    -}

    -------------------------------------------------------------------------------
    -- Data output utils follow..
    -------------------------------------------------------------------------------

    data OutputValue = SI8  Int8
                     | SI16 Int16
                     | SI32 Int32
                     | SI64 Int64
                     | UI8  Word8
                     | UI16 Word16
                     | UI32 Word32
                     | UI64 Word64
                     | OutputValues [OutputValue]
                     | OutputString String
                     | OutputNothing
        deriving Show;
    
    --byte size of an output value
    sizeOfOutputValue :: OutputValue -> Int;
    sizeOfOutputValue (SI8  _) = 1;
    sizeOfOutputValue (SI16 _) = 2; 
    sizeOfOutputValue (SI32 _) = 4;
    sizeOfOutputValue (SI64 _) = 8;
    sizeOfOutputValue (UI8  _) = 1;
    sizeOfOutputValue (UI16 _) = 2;
    sizeOfOutputValue (UI32 _) = 4;
    sizeOfOutputValue (UI64 _) = 8;
    sizeOfOutputValue (OutputValues vv) = sizeOfOutputValues vv;
    sizeOfOutputValue (OutputString s)  = length s;
    sizeOfOutputValue OutputNothing     = 0;

    --total size of an array of output values
    sizeOfOutputValues :: [OutputValue] -> Int;
    sizeOfOutputValues [] = 0;
    sizeOfOutputValues (v:vs) = (sizeOfOutputValue v) + (sizeOfOutputValues vs);
    
    --write an output value
    writeOutputValue :: OutputValue -> String;
    writeOutputValue (SI8  v) = writeSI8  v;
    writeOutputValue (SI16 v) = writeSI16 v;
    writeOutputValue (SI32 v) = writeSI32 v;
    writeOutputValue (SI64 v) = writeSI64 v;
    writeOutputValue (UI8  v) = writeUI8  v;
    writeOutputValue (UI16 v) = writeUI16 v;
    writeOutputValue (UI32 v) = writeUI32 v;
    writeOutputValue (UI64 v) = writeUI64 v;
    writeOutputValue (OutputValues vv) = writeOutputValues vv;
    writeOutputValue (OutputString s)  = s;
    writeOutputValue OutputNothing     = "";
    
    --write an array of output values
    writeOutputValues :: [OutputValue] -> String;
    writeOutputValues [] = "";
    writeOutputValues (v:[]) = writeOutputValue v;
    writeOutputValues (v:vs) = (writeOutputValue v) ++ (writeOutputValues vs);

    -- | Write a list of items prefixed by the 16 bit length
    writeItems :: [a] -> (a -> OutputValue) -> OutputValue;
    writeItems arr writer = OutputValues ((UI16 $ i2i $ length arr):(map writer arr));
  
    writeSI8  = writeUI8  . si8toui8;
    writeSI16 = writeUI16 . si16toui16;
    writeSI32 = writeUI32 . si32toui32;
    writeSI64 = writeUI64 . si64toui64;

    si8toui8 :: Int8 -> Word8;
    si8toui8 i = if i < 0 then i2i (256 + (toInteger i)) else i2i i;

    si16toui16 :: Int16 -> Word16;
    si16toui16 i = if i < 0 then i2i (0x10000 + (toInteger i)) else i2i i;

    si32toui32 :: Int32 -> Word32;
    si32toui32 i = if i < 0 then i2i (0x100000000 + (toInteger i)) else i2i i;

    si64toui64 :: Int64 -> Word64;
    si64toui64 i = if i < 0 then i2i (0x10000000000000000 + (toInteger i)) else i2i i;


    writeUI8 :: Word8 -> String;
    writeUI8 i = [getByteAsChar i 0];
    
    writeUI32 :: Word32 -> String;
    writeUI32 i = [getByteAsChar i 3, getByteAsChar i 2, getByteAsChar i 1, getByteAsChar i 0 ];

    writeUI16 :: Word16 -> String;
    writeUI16 i = [getByteAsChar i 1, getByteAsChar i 0 ];

    writeUI64 :: Word64 -> String;
    writeUI64 i = [getByteAsChar i 7, getByteAsChar i 6, getByteAsChar i 5, getByteAsChar i 4,
                   getByteAsChar i 3, getByteAsChar i 2, getByteAsChar i 1, getByteAsChar i 0 ];

    convertToUTF8 :: String -> String;
    convertToUTF8 s = concat $ map writeUTF8Char s;
    
    writeUTF8Char :: Char -> String;
    writeUTF8Char c = let i = ord c in
                      if i == 0 then writeUI16 0xc080
                      else if i >= 1 && i <= 0x7f then [c]
                      else if i >= 0x80 && i <= 0x07FF then 
                          writeUI16 $ i2i ( 0xc080 + ((i `shiftL` 2) .&. 0xff00) + (i .&. 0x3f))
                      else writeUI32 $ i2i ( 0xe08080 + ((i `shiftL` 4) .&. 0xff0000) + ((i `shiftL` 2) .&. 0xff00) + (i .&. 0x3f));
    
    --get a given byte
    getByte :: (Bits a, Enum a) => a -> Int -> Int;
    getByte i b = fromEnum((i `shiftR` (8*b)) .&. 0xff);
    
    getByteAsChar :: (Bits a, Enum a) => a -> Int -> Char;
    getByteAsChar i b = chr $ getByte i b;
    
    
    bytesToString :: [Word8] -> String;
    bytesToString [] = [];
    bytesToString (w:ws) = (chr $ fromEnum w):(bytesToString ws);
    
    --Convert from one int type to another
    i2i :: (Integral a, Num b) => a -> b;
    i2i = fromIntegral; --fromInteger $ toInteger a;
    
    -------------------------------------------------------------------------------
    -- IEEE 754 utils follow..
    -------------------------------------------------------------------------------
    
    --convert a float to a number (>=1,<2) and a binary exponent
    findSignificandAndExpo :: (Integral a, RealFrac f) => f -> (f,a);
    findSignificandAndExpo f = let (m,e) = findSignificandAndExpo' f 1 in (m,fromIntegral e);
    findSignificandAndExpo' :: (RealFrac f) => f -> Int -> (f,Int);
    findSignificandAndExpo' f e = let m = f / (2 ^^ e)
                                  in if (m >= 1) && (m < 2) then (m,e)
                                     else findSignificandAndExpo' f (if m < 1 then e-1 else e+1);

    --determine the bits for a fraction                                     
    fracToBits :: (RealFrac f) => f -> Int {-num bits-} -> [Bool];
    fracToBits f b = fracToBits' f b 1 0;
    fracToBits' :: (RealFrac f) => f -> Int {-num bits-} -> Int {-current bit-} -> f {-value so far-} -> [Bool];
    fracToBits' f maxB b v = if b > maxB then []
                              else let newVal = v + (valueOfBit b) 
                                   in if newVal > f then (
                                          if (b == maxB) && (newVal - f < f - v) then [True]
                                          else False : (fracToBits' f maxB (b+1) v)
                                      )
                                      else True : (fracToBits' f maxB (b+1) newVal);

    valueOfBit :: (RealFrac f) => Int -> f;
    valueOfBit b = 2 ^^ (-b);

    --Convert a bit array to a number
    bitsToNum :: (Num a) => [Bool] -> a;
    bitsToNum b = bitsToNum' b 0;
    bitsToNum' :: (Num a) => [Bool] -> a -> a;
    bitsToNum' [] v = v;
    bitsToNum' (b:bs) v = bitsToNum' bs ((v * 2) + (if b then 1 else 0));

    floatToIEEE754 :: Float -> Word32;
    floatToIEEE754 0 = 0;
    floatToIEEE754 f | isInfinite     f = if f < 0 then 0xff800000 else 0x7f800000
                     | isNaN          f = 0x7fffffff
                     | isNegativeZero f = 0x80000000
                     | otherwise =    
                           let sign = signum f;
                               val  = abs f;
                            in let (mantissa,expo) = findSignificandAndExpo val;
                               in let (_,frac) = properFraction mantissa
                                  in (if sign == -1 then 0x80000000 else 0) +
                                     (((expo+127) .&. 0xff) `shiftL` 23) +
                                     ((bitsToNum $ fracToBits frac 23) .&. 0x7fffff);

    doubleToIEEE754 :: Double -> Word64;
    doubleToIEEE754 0 = 0;
    doubleToIEEE754 d | isInfinite     d = if d < 0 then 0xfff0000000000000 else 0x7ff0000000000000
                      | isNaN          d = 0x7fffffffffffffff
                      | isNegativeZero d = 0x8000000000000000
                      | otherwise =    
                            let sign = signum d;
                                val  = abs d;
                            in let (mantissa,expo) = findSignificandAndExpo val;
                               in let (_,frac) = properFraction mantissa
                                  in (if sign == -1 then 0x8000000000000000 else 0) +
                                     (((expo+1023) .&. 0x7ff) `shiftL` 52) +
                                     ((bitsToNum $ fracToBits frac 52) .&. 0xfffffffffffff);
}