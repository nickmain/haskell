-- | Utility functions for the JavaType datatype
module JVM.Types.Utils (

	module JVM.Types,
	module JVM.Types.Utils

) where {

	import JVM.Types;

    -- | Get the JVM type string for a JavaType
    jtypeToSig :: JavaType -> String;
    jtypeToSig jtype = case jtype of {
        (Primitive prim)  -> primitiveToSig prim;
        JVoid             -> "V";    
        (JReference name) -> "L" ++ (classNameToJVMFormat name) ++ ";";
        (JArray t )   -> "[" ++ (jtypeToSig t);
    };

    -- | Get the JVM type string for a primitive type
    primitiveToSig :: PrimitiveType -> String;
    primitiveToSig ptype = case ptype of {
        JByte    -> "B";
        JChar    -> "C";
        JShort   -> "S"; 
        JBoolean -> "Z";
        JInt     -> "I";
        JLong    -> "J";
        JFloat   -> "F";
        JDouble  -> "D";
    };

    -- | True if the type is internally represented by an int
    isIntType :: PrimitiveType -> Bool;
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
        (Primitive  _) -> True;
        otherwise      -> False;
    };    

    -- | Get the JVM signature string for a Signature
    signatureToJVMSig :: MethodSig -> String;
    signatureToJVMSig (MethodSig argTypes returnType) = "(" ++ (concat $ map jtypeToSig argTypes) ++ ")" ++ (jtypeToSig returnType);
 
    -- | Get a Signature from the JVM form
    signatureFromJVMSig :: String -> MethodSig;
    signatureFromJVMSig ('(':cc) = let (params,(')':retType)) = span (/=')') cc 
                                    in MethodSig (readParams params) (jtypeFromSig retType);
    signatureFromJVMSig s = MethodSig [] $ JReference ("**INVALID SIGNATURE " ++ s);

    -- | Read types from param list
    readParams :: String -> [JavaType];
    readParams [] = [];
    readParams s  = let (typ,rest) = readType s in typ:(readParams rest);

    -- | Read a type from a string and also return the remainder
    readType :: String -> (JavaType,String);
    readType s@(c:cc) = case c of {
        '[' -> let (elType,rest) = readType cc
                in (JArray elType, rest);
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
        '['       -> JArray (jtypeFromSig cc);
        otherwise -> JReference (jvmFormatToClassName s);
    };

    -- | Get a JavaType from a sig
    jtypeFromSig :: String -> JavaType;
    jtypeFromSig [] = JReference "<EMPTY TYPE SIGNATURE>"; 
    jtypeFromSig (c:[]) = case c of {
        'B' -> Primitive JByte;
        'C' -> Primitive JChar;
        'S' -> Primitive JShort;
        'Z' -> Primitive JBoolean;
        'I' -> Primitive JInt;
        'J' -> Primitive JLong;
        'F' -> Primitive JFloat;
        'D' -> Primitive JDouble;
        'V' -> JVoid;
        otherwise -> JReference [c]; --ASSUME type is a class name
    };
    jtypeFromSig s@(c:cc) = case c of {
        '[' -> JArray (jtypeFromSig cc);
        'L' -> JReference $ jvmFormatToClassName $ takeWhile (/= ';') cc;
        otherwise -> JReference (jvmFormatToClassName s); --ASSUME type is a class name
    };    
    
    -- | Drop the given number of dimensions from any array
    dropArrayDims :: (Integral i) => i -> JavaType -> JavaType;
    dropArrayDims 0 typ          = typ;
    dropArrayDim  n (JArray typ) = dropArrayDims (n - 1) typ;
}
