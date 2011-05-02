{-# OPTIONS_GHC -fglasgow-exts #-}
module Test where {

    import JVM.ClassFile.IO.Parser;
   -- import Epistem.SimpleLanguage;
    import Epistem.SimpleLanguage.Show;
    import JVM.ClassFile.ToSimpleLang;
    import Epistem.Util.GraphUtils;
    import JVM.ClassFile.Contents;
    import Data.Graph.Inductive;

    {-
    test fp = do {
        (Right r) <- parseClassFile fp;
        putStrLn $ show (translateClass r);
    }; 
    -}
    
    test1 = test "/Users/nickmain/Documents/workspace/epistem/JavaCode/eclipse-build/test/HelloWorld.class";
    
    testC = do {
        (Right cf) <- parseClassFile "/Users/nickmain/Documents/workspace/epistem/JavaCode/eclipse-build/test/HelloWorld.class";
        
        let cg = codeGraph $ classMethods cf
         in do {
                putStrLn cg;
                writeFile "/Users/nickmain/Desktop/test.dot" cg
            }
    }; 
    
    codeGraph :: [ClassMethod] -> String;
    codeGraph cc = unlines
                 $ map removeInternalQuotes
                 $ lines
                 $ graphviz' 
                 $ bytecode
                 $ head
                 $ map (\(Code ca)-> ca)
                 $ filter (\ma->case ma of { Code ca -> True; otherwise -> False })
                 $ concatMap methodAttributes 
                 $ filter (\cm->(methodName cm) == "toString") cc;
    
    test fp = do {
        cf <- parseClassFile fp;
        putStrLn $ show cf;
    }; 

	data Expr = forall a. Foo a => Expr a;
	
	class Foo a where {
		bar :: a -> String;
	    baz :: String -> a;
	};
	
	instance Foo Int where {
		bar i = show i;
		baz s = read s;
	};
	
	foo :: Expr -> String;
	foo (Expr a) = bar a;
	
	test2 = putStrLn $ foo (Expr (4::Int));
}
