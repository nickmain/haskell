-- | A Show instance for JVMClass
module JVM.ClassFile.Contents.Show where {

    import JVM.ClassFile.Contents;
    import Epistem.Util.IndentingPrinter;
    import Data.List;
                              
    instance Show JVMClass where {
        show cf = prettyPrint $ do {
            prnt "class ";
            prnt (className cf);
            
            -- TODO: flags       
            
            if (superclassName cf) /= "" 
                then do {
                    indent;
                    newline;                    
                    prnt "extends ";
                    prnt (superclassName cf);
                    unindent
                }
                else nada;
        
            if (interfaceNames cf) == [] 
                then nada
                else do {
                    indent;
                    newline;
                    prnt "implements ";
                    prnt $ concat $ intersperse ", " (interfaceNames cf);
                    unindent
                };
               
            prntLn " {";
            newline;
            indent;
            
            mapM_ printField (classFields cf);
            newline;
            mapM_ printMethod (classMethods cf);
            
            newline;            
            mapM_ prntLn (map show (classAttributes cf));
                        
            unindent;
            newline;
            prntLn "}"
        }        
    };   
    
    instance Show ClassMethod where {
        show = prettyPrint . printMethod; 
    }; 

    -- print a method  
    printMethod :: ClassMethod -> PrinterState ();
    printMethod m = do {
        -- TODO: flags
        prnt $ methodName m;
        prnt $ show $ methodSig m;

        let excps = thrownExceptions m
         in if null excps 
            then nada
            else do {
                     newline;
                     prnt "    throws ";
                     mapM_ prnt (intersperse ", " $ map (\(JReference name)->name) excps);
                 };

        let attrs = methodAttributes m
         in if null attrs 
            then newline
            else do {
                     newline;
                     indent;
                     prntLn "attributes {";
                     indent;
                     mapM_ prntLn (map show attrs);
                     unindent;
                     prntLn "}";
                     unindent
                 }
    };
    
    instance Show ClassField where {
        show = prettyPrint . printField; 
    }; 

	-- print a field    
    printField :: ClassField -> PrinterState ();
    printField f = do {
        -- TODO: flags
    	prnt $ show $ fieldType f;
    	prnt " ";
        prnt $ fieldName f;
        
        let attrs = fieldAttributes f 
         in if null attrs 
            then newline
            else do {
                     prntLn " {";
                     indent;
                     mapM_ prntLn (map show attrs);
                     unindent;
                     prntLn "}"
                 }
    };
}
