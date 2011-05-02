-- | The Code attribute
module JVM.ClassFile.Contents.CodeAttribute where {

    import JVM.Types;
    import JVM.ClassFile.Contents.Types;
    import Data.Word;
    import Data.Int;
    import Epistem.Util.IndentingPrinter;
	import Data.Graph.Inductive;
    
    -- The JVM Code attribute
    data CodeAttribute = CodeAttribute { 
                             maxStack, maxLocals :: Word16, 
                             bytecode       :: InstructionGraph,
                             exceptionTable :: [ExceptionTableEntry],
                             codeAttributes :: [Attribute]
                         };

	-- Instruction graph
	type InstructionGraph = Gr Instruction CodeFlow;
   
	-- Edge types
	data CodeFlow = NormalFlow
	              | DefaultCase
	              | Case Int32
	              | Conditional BranchCondition
                  | SubroutineCall
                  
                  -- "Structured" flows follow..
                  | CatchFlow JavaClassName
                  | BlockStart
                  | BlockEnd
                  ;

    data ExceptionTableEntry = ExceptionTableEntry { 
        startPC, endPC, handlerPC :: Node, 
        catchType :: JavaClassName 
    } deriving (Show,Eq);
    
    type VarIndex = Int;
    
    -- | Abstracted JVM Instructions
    data Instruction = NoOp
                     | PushNull      
                     | PushConstant ConstantValue          
                     | PushVar      VarIndex
                     | StoreVar     VarIndex
                     | PushElement                
                     | StoreElement
                     
                     | Pop    Int {-count-}
                     | Dup    Int {-count-} Int {-skip count-}
                     | Swap
                     
                     | Add
                     | Sub
                     | Mul
                     | Div
                     | Rem
                     | Neg
                     | ShiftLeft    
                     | ShiftRight
                     | ShiftRightU              
                     | And
                     | Or                    
                     | Xor
                     | Increment VarIndex Int
                     | Compare
                     | CompareL
                     | CompareG
                     
                     | Convert    PrimitiveType {-to-}
                     | CheckCast  JavaType
                     | InstanceOf JavaType

                     | Branch
                     | Switch
                     | Return

                     | SubCall
                     | SubRet  VarIndex        
                     
                     | GetStatic       FieldRef
                     | PutStatic       FieldRef
                     | GetField        FieldRef
                     | PutField        FieldRef
                     | InvokeVirtual   MethodRef
                     | InvokeSpecial   MethodRef
                     | InvokeStatic    MethodRef
                     | InvokeInterface MethodRef
                     
                     | New             JavaClassName
                     | NewArray        JavaType Int {-num dims-}
                     | ArrayLength
                     
                     | Throw
                     
                     | MonitorEnter    
                     | MonitorExit      
                     
                     -- "Structured" pseudo-instructions follow..
                     | Try
                     | Catch
                     
        deriving Show;                             
        
    -- Condition types
    data BranchCondition = IfEQ0
                         | IfNE0
                         | IfLT0
                         | IfGE0
                         | IfGT0
                         | IfLE0

                         | IfNull
                         | IfNotNull
                     
                         | IfEQ
                         | IfNE
                         | IfLT
                         | IfGE
                         | IfGT
                         | IfLE
		deriving Show;
        
    instance Show CodeAttribute where {
        show = prettyPrint . printCode; 
    };
    
    printCode :: CodeAttribute -> PrinterState ();
    printCode c = do {
        prntLn "{";
        indent;
        
        let ig = bytecode c
          in printInstructions $ map (context ig) $ nodes ig;

        let exs = exceptionTable c
          in if null exs 
                 then nada
                 else do {
                    newline;
                    mapM_ printExHandler exs;                 
                 };

        unindent;
        prntLn "}";    
    };
    
    printExHandler :: ExceptionTableEntry -> PrinterState ();
    printExHandler (ExceptionTableEntry s e h t) =  do {
        prnt "try ( ";
        prnt (show s);
        prnt " .. ";
        prnt (show e);        
        prnt " ) catch( ";
        prnt t;
        prnt " ) -> ";
        prntLn (show h); 
    };
    
    printInstructions :: [Context Instruction CodeFlow] -> PrinterState ();
    printInstructions [] = nada;
    printInstructions (ctxt:[]) = printInstruction ctxt (-1::Node);
    printInstructions (ctxt:n@(_,next,_,_):cc) = (printInstruction ctxt next) 
                                               >> (printInstructions (n:cc));    
    
    printInstruction :: Context Instruction CodeFlow 
                     -> Node   -- next instruction
                     -> PrinterState ();
    printInstruction (_, node, ins, out) next = do {
        prnt $ show node;
        prnt ": ";
        prnt (show ins);
        
        mapM_ (\(cf,n)->case cf of {
            NormalFlow        -> if n /= next then prnt (" -> " ++ (show n)) else nada;
            DefaultCase       -> prnt $ "\n        default -> " ++ (show n);
            Case v            -> prnt $ "\n        case " ++ (show v) ++ " -> " ++ (show n);
            Conditional c     -> prnt $ " " ++ (show c) ++ " -> " ++ (show n);
            SubroutineCall    -> prnt $ " -> " ++ (show n);
            otherwise         -> prnt $ show cf;
        }) out;
        
        newline
    };
    
    instance Show CodeFlow where {
        show cf = case cf of {        
            NormalFlow       -> "";
            DefaultCase      -> "default";
            (Case         v) -> "case " ++ (show v);
            (Conditional  c) -> show c;
            SubroutineCall   -> "subcall";
            (CatchFlow    e) -> "catch " ++ e;
            BlockStart       -> "start";
            BlockEnd         -> "end";
        }
    };
}