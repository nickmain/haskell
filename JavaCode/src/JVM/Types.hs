-- | Data types for Java types.
module JVM.Types where {

    import Data.List;

    -- | Java class name is a String in dot (not slash) format
    type JavaClassName = String;

    -- | Java types
    data JavaType = Primitive PrimitiveType
                  | JReference JavaClassName
                  | JArray JavaType
                  | JVoid
        deriving (Eq);

    -- | Primitive Types
    data PrimitiveType = JByte | JChar | JShort | JBoolean 
                       | JInt  | JLong | JFloat | JDouble
        deriving (Eq);

    -- | A method signature
    data MethodSig = MethodSig [JavaType] -- ^ parameter types
                               JavaType   -- ^ return type
        deriving (Eq);

    -- | Name of java-lang-object
    javaLangObject = "java.lang.Object";        

    instance Show MethodSig where {
        show (MethodSig p r) = "(" ++ (concat $ intersperse "," (map show p)) ++ "):" ++ (show r);
    };
    
    instance Show JavaType where {
        show t = case t of {
            (Primitive  t) -> show t;
            (JArray t    ) -> (show t) ++ "[]";
            (JReference n) -> n;
            (JVoid)        -> "void";
        };
    };
    
    instance Show PrimitiveType where {
        show p = case p of {
            JByte    -> "byte";
            JChar    -> "char";
            JShort   -> "short";
            JBoolean -> "boolean";
            JInt     -> "int";
            JLong    -> "long";
            JFloat   -> "float";
            JDouble  -> "double";
        };
    };
}