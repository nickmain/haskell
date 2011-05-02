-- | Class structures in the Simple Language
module Epistem.SimpleLanguage.Class where {

    import Epistem.SimpleLanguage.Types;
    import Epistem.SimpleLanguage.Code;
    import Data.Int;

    data ClassType = NormalClass
                   | InterfaceClass
                   | AnnotationClass
                   | EnumerationClass
                   ;

    data Class = Class {
                     className          :: String,
                     classType          :: ClassType,
                     classVisibility    :: Visibility,
                     classExtensibility :: Extensibility,
                     superclassName     :: String,     -- empty for none
                     classInterfaces    :: [String],
                     classFields        :: [Field],
                     classMethods       :: [Method],
                     classConstructors  :: [Constructor],
                     staticInitializer  :: (Maybe StaticInitializer),
                     classAnnotations   :: [Annotation]
                 };
               
    data Visibility    = Public | Private  | Protected | Package;
    data Extensibility = Final  | Abstract | Extensible;

    -- | A field
    data Field = Field {
                     fieldName        :: String,
                     fieldType        :: Type,
                     fieldVisibility  :: Visibility,
                     fieldAttrs       :: FieldAttrs,
                     fieldValue       :: (Maybe Constant),
                     fieldAnnotations :: [Annotation]
                 };
                
    type FieldAttrs = (Bool,Bool,Bool); -- ^ (static,final,transient)
    isStatic    f = let (b,_,_) = fieldAttrs f in b;
    isFinal     f = let (_,b,_) = fieldAttrs f in b;
    isTransient f = let (_,_,b) = fieldAttrs f in b;
                       
    -- | A method
    data Method = Method {
                      methodName          :: String,
                      methodType          :: Type,
                      methodParams        :: [Param],
                      methodVisibility    :: Visibility,
                      methodExtensibility :: Extensibility,
                      methodSynchronized  :: Bool,
                      methodCode          :: (Maybe Code),  -- Nothing if abstract or native method
                      methodAnnotations   :: [Annotation]
                  };
                
    -- | A constructor
    data Constructor = Constructor {
                           constructorParams     :: [Param],
                           constructorVisibility :: Visibility,
                           constructorCode       :: Code
                       };
    
    -- | A static initializer is just code    
    type StaticInitializer = Code;
    
    -- | A method parameter 
    data Param = Param {
                     paramType        :: Type,
                     paramName        :: String,
                     paramAnnotations :: [Annotation]
                 };                       
    
    -- | An annotation (type name [(name,value)]
    data Annotation = Annotation String [(String,AnnotValue)];

    -- | An annotation element value
    data AnnotValue = AnnotValByte   Int8
                    | AnnotValBool   Bool
                    | AnnotValShort  Int16
                    | AnnotValChar   Char
                    | AnnotValInt    Int32
                    | AnnotValLong   Int64
                    | AnnotValFloat  Float
                    | AnnotValDouble Double
                    | AnnotValString String
                    | AnnotValEnum   String String -- ^ Type name, Const name
                    | AnnotValType   Type
                    | AnnotValAnnot  Annotation
                    | AnnotValArray  [AnnotValue]
                    ;
}
