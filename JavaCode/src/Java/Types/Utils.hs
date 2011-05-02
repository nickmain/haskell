-- | Utility functions for the JavaType datatype
module Java.Types.Utils (

	module Java.Types,
	module Java.Types.Utils

) where {

	import Java.Types;

    -- | Get the JVM type string for a JavaType
    jtypeToSig :: JavaType -> String;
    jtypeToSig jtype = case jtype of {
        JByte    -> "B";
        JChar    -> "C";
        JShort   -> "S"; 
        JBoolean -> "Z";
        JInt     -> "I";
        JLong    -> "J";
        JFloat   -> "F";
        JDouble  -> "D";
        JVoid    -> "V";    
        (JReference name) -> "L" ++ (classNameToJVMFormat name) ++ ";";
        (JArray t 1   )   -> "[" ++ (jtypeToSig t);
        (JArray t dims)   -> "[" ++ (jtypeToSig $ JArray t (dims-1));
    };

    -- | True if the type is internally represented by an int
    isIntType :: JavaType -> Bool;
    isIntType t = case t of {
        JByte     -> True;
        JChar     -> True;
        JShort    -> True;
        JBoolean  -> True;
        JInt      -> True;
        otherwise -> False;
    };

    -- | True if the type is a primitive type
    isPrimitiveType :: JavaType -> Bool;
    isPrimitiveType t = case t of {
        JVoid          -> False;
        (JReference _) -> False;
        (JArray   _ _) -> False;
        otherwise      -> True;
    };    

    -- | Get the JVM signature string for a Signature
    signatureToJVMSig :: Signature -> String;
    signatureToJVMSig (Signature argTypes returnType) = "(" ++ (concat $ map jtypeToSig argTypes) ++ ")" ++ (jtypeToSig returnType);
 
    -- | Get a Signature from the JVM form
    signatureFromJVMSig :: String -> Signature;
    signatureFromJVMSig ('(':cc) = let (params,(')':retType)) = span (/=')') cc 
                                    in Signature (readParams params) (jtypeFromSig retType);
    signatureFromJVMSig s = Signature [] $ JReference ("**INVALID SIGNATURE " ++ s);

    -- | Read types from param list
    readParams :: String -> [JavaType];
    readParams [] = [];
    readParams s  = let (typ,rest) = readType s in typ:(readParams rest);

    -- | Read a type from a string and also return the remainder
    readType :: String -> (JavaType,String);
    readType s@(c:cc) = case c of {
        '[' -> let (dims,rest) = span (=='[') s 
                in (\(elType,rest)->(JArray elType (length dims), rest)) $ readType rest;
        'L' -> let (cname,rest) = span (/=';') cc in (JReference (jvmFormatToClassName cname), tail rest);
        otherwise -> (jtypeFromSig [c],cc);
    };

    -- | Convert dot format class name to slash format
    classNameToJVMFormat :: JavaClassName -> JavaClassName;
    classNameToJVMFormat = map (\c->if c == '.' then '/' else c);

    -- | Convert slash format class name to dot format
    jvmFormatToClassName :: JavaClassName -> JavaClassName;
    jvmFormatToClassName = map (\c->if c == '/' then '.' else c);

    -- | Get a JavaType from a class name (which might be an array)
    jtypeFromClassName :: String -> JavaType;
    jtypeFromClassName [] = JReference "<EMPTY CLASS NAME>";
    jtypeFromClassName s@(c:cc) = case c of {
        '[' -> case jtypeFromSig cc of {
                   (JArray jtype dims) -> JArray jtype (dims+1);
                   jtype               -> JArray jtype 1;
               };
        otherwise -> JReference (jvmFormatToClassName s);
    };

    -- | Get a JavaType from a sig
    jtypeFromSig :: String -> JavaType;
    jtypeFromSig [] = JReference "<EMPTY TYPE SIGNATURE>"; 
    jtypeFromSig (c:[]) = case c of {
        'B' -> JByte;
        'C' -> JChar;
        'S' -> JShort;
        'Z' -> JBoolean;
        'I' -> JInt;
        'J' -> JLong;
        'F' -> JFloat;
        'D' -> JDouble;
        'V' -> JVoid;
        otherwise -> JReference [c]; --ASSUME type is a class name
    };
    jtypeFromSig s@(c:cc) = case c of {
        '[' -> case jtypeFromSig cc of {
                   (JArray jtype dims) -> JArray jtype (dims+1);
                   jtype               -> JArray jtype 1;
               };
        'L' -> JReference $ jvmFormatToClassName $ takeWhile (/= ';') cc;
        otherwise -> JReference (jvmFormatToClassName s); --ASSUME type is a class name
    };

}
