-- | Class, field and method flags
module Java.VM.Class.File.Flags where {

    import Data.Bits;

    -- | Test whether a class, field or method flag is set
    flagIs :: Bits a => Int  -- ^ the flag bit number              
           -> a              -- ^ the flags
           -> Bool;
    flagIs b f = testBit f b;

    -- | Set the given flags for a class, field or method.
    setFlags :: Bits a => a  -- ^ the existing flags
             -> [Int]        -- ^ the bit numbers to set
             -> a;           -- ^ the updated flags
    setFlags f [] = f;
    setFlags f (b:bb) = setFlags (setBit f b) bb;

    -- | Flag bit numbers
    _PUBLIC       = 0 ::Int;
    _PRIVATE      = 1 ::Int;
    _PROTECTED    = 2 ::Int;
    _STATIC       = 3 ::Int;
    _FINAL        = 4 ::Int;
    _SYNCHRONIZED = 5 ::Int;
    _SUPER        = 5 ::Int;
    _BRIDGE       = 6 ::Int;
    _VOLATILE     = 6 ::Int;
    _VARARGS      = 7 ::Int;
    _TRANSIENT    = 7 ::Int;
    _NATIVE       = 8 ::Int;
    _INTERFACE    = 9 ::Int;
    _ABSTRACT     = 10::Int;     
    _STRICT       = 11::Int;
    _SYNTHETIC    = 12::Int;     
    _ANNOTATION   = 13::Int;     
    _ENUM         = 14::Int;    

    -- | Show a set of class flags
    showClassFlags :: Bits a => a -> String;
    showClassFlags = showFlags classFlagStrings;

    -- | Show a set of method flags
    showMethodFlags :: Bits a => a -> String;
    showMethodFlags = showFlags methodFlagStrings;

    -- | Show a set of field flags
    showFieldFlags :: Bits a => a -> String;
    showFieldFlags = showFlags fieldFlagStrings;


    showFlags :: Bits a => [(Int,String)] -> a -> String;
    showFlags [] _ = [];
    showFlags ((b,s):ss) a =  (if flagIs b a then s ++ " " else "")
                           ++ (showFlags ss a);

    -- | Strings for class flags
    classFlagStrings = [ 
        (_PUBLIC      ,"public"),
        (_STATIC      ,"static"),
        (_FINAL       ,"final"),
        (_SUPER       ,"super"),
        (_INTERFACE   ,"interface"),
        (_ABSTRACT    ,"abstract"),     
        (_SYNTHETIC   ,"synthetic"),     
        (_ANNOTATION  ,"annotation"),     
        (_ENUM        ,"enum")
    ];

    -- | Strings for method flags
    methodFlagStrings = [ 
        (_PUBLIC      ,"public"),
        (_PRIVATE     ,"private"),
        (_PROTECTED   ,"protected"),
        (_STATIC      ,"static"),
        (_FINAL       ,"final"),
        (_SYNCHRONIZED,"sync"),
        (_BRIDGE      ,"bridge"),
        (_VARARGS     ,"varargs"),
        (_NATIVE      ,"native"),
        (_ABSTRACT    ,"abstract"),     
        (_STRICT      ,"strict"),
        (_SYNTHETIC   ,"synthetic")
    ];

    -- | Strings for field flags
    fieldFlagStrings = [ 
        (_PUBLIC      ,"public"),
        (_PRIVATE     ,"private"),
        (_PROTECTED   ,"protected"),
        (_STATIC      ,"static"),
        (_FINAL       ,"final"),
        (_VOLATILE    ,"volatile"),
        (_TRANSIENT   ,"transient"),
        (_SYNTHETIC   ,"synthetic"),     
        (_ENUM        ,"enum")
    ];
}
