-- | Data types for Java types.
module Java.Types where {

    -- | Java class name is a String in dot (not slash) format
    type JavaClassName = String;

    -- | Java types
    data JavaType = Primitive PrimitiveType
                  | JReference JavaClassName
                  | JArray JavaType Int -- ^ array of type with number of dimensions
                  | JVoid
        deriving (Show,Eq);

    -- | Primitive Types
    data PrimitiveType = JByte | JChar | JShort | JBoolean 
                       | JInt  | JLong | JFloat | JDouble
        deriving (Show, Eq);

    -- | A method signature
    data Signature = Signature [JavaType] -- ^ parameter types
                               JavaType   -- ^ return type
        deriving (Show,Eq);
        

}