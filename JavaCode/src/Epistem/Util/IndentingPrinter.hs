-- | State monad for pretty printing with indentations
module Epistem.Util.IndentingPrinter where {

    import Control.Monad.State;
    import Data.List(intersperse);
    
    -- | The printing state
    data Printer = S String  -- accumulated output
                     Int     -- indent level
                     String  -- indent chars
                     Bool    -- at start of line ?
                   ;
                     
    -- | The state monad
    type PrinterState = State Printer;

    -- | Initial printer with indent string of 4 spaces
    initPrinter :: Printer;
    initPrinter = S "" 0 "    " True;

    -- | Print nothing
    nada :: PrinterState ();
    nada = return ();

    -- | Increase indent
    indent :: PrinterState ();
    indent = do {
        (S s i c b) <- get;
        put (S s (i+1) c b)
    };
    
    -- | Decrease indent
    unindent :: PrinterState ();
    unindent = do {
        (S s i c b) <- get;
        if i > 0 
            then put (S s (i-1) c b)
            else return ()
    };
    
    -- | Print all items in an array
    prntAll :: (Show a) => [a] -> PrinterState ();
    prntAll aa = mapM_ prnt (map show aa); 
    
    -- | Print all items in an array on separate lines
    prntLnAll :: (Show a) => [a] -> PrinterState ();
    prntLnAll aa = mapM_ prntLn (map show aa); 
    
    -- | Print all items in an array, with a given separator
    prntAllSep :: (Show a) => String -> [a] -> PrinterState ();
    prntAllSep d aa = mapM_ prnt (intersperse d $ map show aa);     
    
    -- | Print a string and then newline
    prntLn :: String -> PrinterState ();
    prntLn s = do {
        prnt s;
        newline
    };
    
    -- | Print a newline
    newline :: PrinterState ();
    newline = prntChr '\n';
    
    -- | Print a string
    prnt :: String -> PrinterState ();
    prnt []     = return ();
    prnt (c:cc) = do {
        prntChr c;
        prnt cc
    };
    
    -- | Print a char
    prntChr :: Char -> PrinterState ();
    prntChr '\n' = do {
        (S out level indent atStart) <- get;
        put (S (out ++ "\n") level indent True)
    };
    prntChr c = do {
        (S out level indent atStart) <- get;
        if atStart 
            then put (S (out ++ (concat $ replicate level indent) ++ [c]) level indent False)
            else put (S (out ++ [c]) level indent False)
    };
    
    -- | Print a string right aligned in a field of the given size
    prntRight :: Int -> String -> PrinterState ();
    prntRight i s = let len = length s
                     in if len >= i
                          then prnt s
                          else do {
                              prnt $ replicate (i-len) ' ';
                              prnt s
                          }; 

    -- | The pretty print runner
    prettyPrint :: PrinterState () -> String;
    prettyPrint ps = let (S s _ _ _) = execState ps initPrinter
                      in s;
        
}
