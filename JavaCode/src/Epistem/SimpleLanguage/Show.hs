-- | Show instances for the SimpleLanguage
module Epistem.SimpleLanguage.Show where {

    import Epistem.SimpleLanguage;
    import Epistem.Util.IndentingPrinter;
    import Data.List;
    import Data.Maybe;

    instance Show Class where {
        show c = prettyPrint $ do {
            
            prnt $ show $ classVisibility c;
            prnt $ show $ classExtensibility c;
            prnt $ show $ classType c;

            prnt (className c);
            
            if (superclassName c) /= "" 
                then do {
                    indent;
                    newline;                 
                    prnt "extends ";
                    prnt (superclassName c);
                    unindent
                }
                else nada;        

            if (classInterfaces c) == [] 
                then nada
                else do {
                    indent;
                    newline;
                    prnt "implements ";
                    prnt $ concat $ intersperse ", " (classInterfaces c);
                    unindent
                };

            prntLn " {";
            newline;
            indent;

            sequence_ $ intersperse newline $ map printField (classFields c);
            newline;
            
            -- TODO: static init
            
            -- TODO: constructors
            
            -- TODO: methods

            -- TODO: annotations

            unindent;
            newline;
            prntLn "}"
        }    
    };
    
    instance Show Field where {
        show = prettyPrint . printField; 
    }; 

    -- print a field    
    printField :: Field -> PrinterState ();
    printField f = do {
        prnt $ show $ fieldVisibility f;

        if isStatic    f then prnt "static "    else nada;
        if isFinal     f then prnt "final "     else nada;
        if isTransient f then prnt "transient " else nada;
        
        prnt $ show $ fieldType f;
        prnt " ";
        prnt $ fieldName f;
        
        case (fieldValue f) of {
            Nothing  -> nada;
            (Just c) -> do {
                prnt " = ";
                prnt $ show c;
            }
        };
        
        let annos = fieldAnnotations f
         in if null annos
               then prntLn ";"
               else printAnnotations annos
        
    };

    printAnnotations :: [Annotation] -> PrinterState ();
    printAnnotations aa = do {
        newline;
        indent;
        prntLn "annotations {";
        indent;
        sequence_ $ intersperse (prntLn ",") $ map printAnno aa;
        unindent;
        newline;
        prnt "}";
        prntLn ";"
    };

    instance Show Annotation where {
        show = prettyPrint . printAnno;    
    };
    
    printAnno :: Annotation -> PrinterState ();
    printAnno (Annotation typ elems) = do {    
        prntLn $ typ ++ " { ";
        indent;
        mapM_ printAnnoElem elems;
        unindent;
        prnt "}";      
    };

    printAnnoElem :: (String,AnnotValue) -> PrinterState ();
    printAnnoElem (name,v) = do {
        prnt name;
        prnt " = ";
        prnt $ show v;
        prntLn ";"
    };

    instance Show AnnotValue where {
        show = prettyPrint . printAnnotValue; 
    };
    
    printAnnotValue :: AnnotValue -> PrinterState ();
    printAnnotValue v = do {        
        case v of {
            (AnnotValByte     i ) -> prnt $ show i;
            (AnnotValBool True  ) -> prnt "true";
            (AnnotValBool False ) -> prnt "false";
            (AnnotValShort    i ) -> prnt $ show i;
            (AnnotValChar     c ) -> prnt $ show c;
            (AnnotValInt      i ) -> prnt $ show i;
            (AnnotValLong     i ) -> prnt $ (show i) ++ "L";
            (AnnotValFloat    f ) -> prnt $ (show f) ++ "f";
            (AnnotValDouble   d ) -> prnt $ show d;
            (AnnotValString   s ) -> prnt $ show s;
            (AnnotValEnum   t n ) -> prnt $ t ++ "." ++ n;
            (AnnotValType     t ) -> prnt $ show t;
            (AnnotValAnnot    a)  -> printAnno a;
            (AnnotValArray   aa ) ->  do {
                prntLn "array { ";
                indent;
                sequence_ $ intersperse (prntLn ",") $ map printAnnotValue aa;
                unindent;
                newline;
                prnt "}";
            }
        }
    };
        
    instance Show Constant where {
        show c = case c of {
            (ConstInt    i  ) -> show i;
            (ConstLong   i  ) -> (show i) ++ "L";
            (ConstFloat  f  ) -> (show f) ++ "f";
            (ConstDouble d  ) -> show d;
            (ConstString s  ) -> show s;
            (ConstChar   c  ) -> show c;
            (ConstBool True ) -> "true";
            (ConstBool False) -> "false";
            (ConstType   t  ) -> show t;
            (ConstNull      ) -> "null";
        }
    };
    
    instance Show Type where {
        show t = case t of  {
            (TypeObject n) -> n;
            (TypeArray  t) -> (show t) ++ "[]";
            (TypeVoid    ) -> "void";
            (TypeInt     ) -> "int";
            (TypeLong    ) -> "long";
            (TypeByte    ) -> "byte";
            (TypeShort   ) -> "short";
            (TypeChar    ) -> "char";
            (TypeBool    ) -> "boolean";
            (TypeFloat   ) -> "float";
            (TypeDouble  ) -> "double";
        }
    };
    
    instance Show ClassType where {
        show t = case t of {
            NormalClass      -> "class ";
            InterfaceClass   -> "interface ";
            AnnotationClass  -> "annotation ";
            EnumerationClass -> "enum ";
        }
    };
    
    instance Show Extensibility where {
        show e = case e of {    
            Final      -> "final ";
            Abstract   -> "abstract ";
            Extensible -> "";
        }
    };
    
    instance Show Visibility where {
        show v = case v of {
            Public    -> "public ";
            Private   -> "private ";
            Protected -> "protected ";
            Package   -> "package ";
        }    
    };
}


