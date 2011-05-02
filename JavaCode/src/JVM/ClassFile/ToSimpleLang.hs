-- | Translator to the SimpleLanguage
module JVM.ClassFile.ToSimpleLang (

    translateClass

) where {

    import Epistem.SimpleLanguage;
    import qualified JVM.ClassFile.Contents as JVM;
    import JVM.ClassFile.Contents.Flags;
    import JVM.Types;
    import Control.Monad.State;
    
    -- | Translate JVM class to SimpleLanguage class
    translateClass :: JVM.JVMClass -> Class;
    translateClass jvmClass = let flags = JVM.classFlags jvmClass in Class {
        className          = JVM.className jvmClass,
        classType          = if flagIs _ANNOTATION flags
                                 then AnnotationClass
                                 else if flagIs _ENUM flags
                                         then EnumerationClass
                                         else if flagIs _INTERFACE flags
                                                  then InterfaceClass
                                                  else NormalClass,
        classVisibility    = if flagIs _PUBLIC flags
                                 then Public
                                 else Package,
        classExtensibility = if flagIs _FINAL flags
                                 then Final
                                 else if flagIs _ABSTRACT flags
                                         then Abstract
                                         else Extensible,
        superclassName     = JVM.superclassName jvmClass,
        classInterfaces    = JVM.interfaceNames jvmClass,
        classFields        = map translateField $ JVM.classFields jvmClass,
        classMethods       = [], -- TODO
        classConstructors  = [], -- TODO
        staticInitializer  = Nothing, -- TODO
        classAnnotations   = map translateAnnotation $ 
                                 getClassAnnotations $ 
                                 JVM.classAttributes jvmClass
    }; 

--    translateCode :: JVM.CodeAttribute -> Code;
--    translateCode ca = 
    
    
    type StackState = State [Type] ;
    
    translateInstruction :: JVM.Instruction -> StackState [Statement];
    translateInstruction i = do {
    
        
        return []
    };
    

    -- Translate Java type to SimpleLanguage type
    jvmTypeToType :: JavaType -> Type;
    jvmTypeToType t = case t of {
        (Primitive  pt) -> case pt of {
                               JByte    -> TypeByte;
                               JChar    -> TypeChar;
                               JShort   -> TypeShort;
                               JBoolean -> TypeBool;
                               JInt     -> TypeInt;
                               JLong    -> TypeLong;
                               JFloat   -> TypeFloat;
                               JDouble  -> TypeDouble
                           };
        (JReference cn) -> TypeObject cn;
        (JArray     jt) -> TypeArray (jvmTypeToType jt);
        JVoid           -> TypeVoid
    };
    
    
    translateField :: JVM.ClassField -> Field;
    translateField f = let flags = JVM.fieldFlags f in Field {
        fieldName        = JVM.fieldName f,
        fieldType        = jvmTypeToType $ JVM.fieldType f,
        fieldVisibility  = getVisibility flags,
        fieldAttrs       = (flagIs _STATIC    flags,
                            flagIs _FINAL     flags,
                            flagIs _TRANSIENT flags),
        fieldValue       = getFieldValue $
                               JVM.fieldAttributes f,
        fieldAnnotations = map translateAnnotation $ 
                               getFieldAnnotations $ 
                               JVM.fieldAttributes f
    };
    
    
    
    getVisibility :: Word16 -> Visibility;
    getVisibility flags | flagIs _PUBLIC flags    = Public
                        | flagIs _PRIVATE flags   = Private
                        | flagIs _PROTECTED flags = Protected
                        | otherwise               = Package;
    
    translateAnnotation :: JVM.Annotation -> Annotation;
    translateAnnotation (JVM.Annotation (JReference cn) elems) = 
        Annotation cn $ map (\(n,v)->(n, translateAnnotValue v)) elems;
        
    translateAnnotValue :: JVM.ElementValue -> AnnotValue;
    translateAnnotValue v = case v of {
        (JVM.ElemValByte   i                ) -> AnnotValByte i;
        (JVM.ElemValBool   b                ) -> AnnotValBool b;
        (JVM.ElemValShort  i                ) -> AnnotValShort i;        
        (JVM.ElemValChar   c                ) -> AnnotValChar  c;
        (JVM.ElemValInt    i                ) -> AnnotValInt   i;
        (JVM.ElemValLong   i                ) -> AnnotValLong  i;
        (JVM.ElemValFloat  f                ) -> AnnotValFloat f;
        (JVM.ElemValDouble d                ) -> AnnotValDouble d;
        (JVM.ElemValString s                ) -> AnnotValString s;
        (JVM.ElemValEnum   (JReference cn) s) -> AnnotValEnum cn s;
        (JVM.ElemValClass  jt               ) -> AnnotValType $ jvmTypeToType jt;
        (JVM.ElemValAnnot  a                ) -> AnnotValAnnot $ translateAnnotation a;
        (JVM.ElemValArray  ee               ) -> AnnotValArray $ map translateAnnotValue ee;
    };
    
    getClassAnnotations :: [JVM.ClassAttribute] -> [JVM.Annotation];
    getClassAnnotations [] = [];
    getClassAnnotations (attr:aa) = case attr of {
        (JVM.ClassAnnotations _ annots) -> annots;
        otherwise                       -> [];
    } ++ (getClassAnnotations aa);
    
    getFieldAnnotations :: [JVM.FieldAttribute] -> [JVM.Annotation];
    getFieldAnnotations [] = [];
    getFieldAnnotations (attr:aa) = case attr of {
        (JVM.FieldAnnotations _ annots) -> annots;
        otherwise                       -> [];
    } ++ (getFieldAnnotations aa);
    
    getFieldValue :: [JVM.FieldAttribute] -> Maybe Constant;
    getFieldValue [] = Nothing;
    getFieldValue (attr:aa) = case attr of {
        (JVM.ConstantField c) -> Just $ translateConstant c;
        otherwise             -> getFieldValue aa;
    };
    
    translateConstant :: JVM.ConstantValue -> Constant;
    translateConstant c = case c of {
        (JVM.ConstInt    i) -> ConstInt i;
        (JVM.ConstFloat  f) -> ConstFloat f;
        (JVM.ConstLong   i) -> ConstLong i;
        (JVM.ConstDouble d) -> ConstDouble d;
        (JVM.ConstString s) -> ConstString s;
        (JVM.ConstClass  t) -> ConstType (jvmTypeToType t);
    };
}
