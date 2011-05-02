-- | The standard attribute names
module JVM.ClassFile.IO.AttributeNames where {

    attrName_Code            = "Code";
    attrName_ConstantValue   = "ConstantValue";
    attrName_Exceptions      = "Exceptions";
    attrName_InnerClasses    = "InnerClasses";
    attrName_EnclosingMethod = "EnclosingMethod";
    attrName_Synthetic       = "Synthetic";
    attrName_Signature       = "Signature";
    attrName_SourceFile      = "SourceFile";
    attrName_Deprecated      = "Deprecated";

    attrName_LineNumberTable        = "LineNumberTable";
    attrName_LocalVariableTable     = "LocalVariableTable";
    attrName_LocalVariableTypeTable = "LocalVariableTypeTable";
    
    attrName_RuntimeVisibleAnnotations            = "RuntimeVisibleAnnotations";
    attrName_RuntimeInvisibleAnnotations          = "RuntimeInvisibleAnnotations";
    attrName_RuntimeVisibleParameterAnnotations   = "RuntimeVisibleParameterAnnotations";
    attrName_RuntimeInvisibleParameterAnnotations = "RuntimeInvisibleParameterAnnotations";
    attrName_AnnotationDefault                    = "AnnotationDefault";

}
