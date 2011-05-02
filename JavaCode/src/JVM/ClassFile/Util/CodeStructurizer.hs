{-# OPTIONS_GHC -fglasgow-exts #-}
-- | Utils to make Code Attribute instructions more structured
module JVM.ClassFile.Util.CodeStructurizer where {

    import JVM.Types;
    import JVM.ClassFile.Contents.Types;
    import Data.Word;
    import Data.Int;
    import Data.Graph.Inductive;
    import JVM.ClassFile.Contents.CodeAttribute;
    import Epistem.Util.GraphUtils;
    
    -- | Remove unconditional branches that serve no purpose and replace them
    --   with branch edges
    removeUnneededBranches :: InstructionGraph -> InstructionGraph;
    removeUnneededBranches ig = shortCircuitNodes ig removeBranch;
    
    removeBranch :: ShortCircuitFn Instruction CodeFlow;
    removeBranch gr ((flow, n):[], _, Branch, (NormalFlow, m):[]) = Just [(n,m,flow)]; 
    removeBranch _ _ = Nothing;
        
    
    -- | Incorporate the exceptions handlers into the bytecode
    incorporateExceptionHandlers :: CodeAttribute -> CodeAttribute;
    incorporateExceptionHandlers c = 
        let newCode = applyGraphFn (bytecode c) 
                                   (\e->runGraph (incorpExHandler e)) 
                                   (exceptionTable c)
         in CodeAttribute { 
                maxStack       = maxStack c,
                maxLocals      = maxLocals c,
                bytecode       = newCode,
                exceptionTable = exceptionTable c,
                codeAttributes = codeAttributes c
            };
        
     --TODO: enumerate all possible situations for tail of try-block   
        
    incorpExHandler :: ExceptionTableEntry -> GraphState Instruction CodeFlow ();
    incorpExHandler (ExceptionTableEntry start end handler catchType) = do {
        incoming <- edgesTo start;
        
        tryNode  <- (Try  , BlockStart) ==> start;
        catNode  <- (Catch, BlockStart) ==> handler;
        
        (tryNode, CatchFlow catchType ) ==> catNode;
        (end,     BlockEnd            ) ==> tryNode;

        -- if the last instruction is a return and all the predecessors come
        -- from within the try-block then make the block-end from that,
        -- otherwise make it from the predecessor
        
        
        
        redirect incoming tryNode        
    };
    
}
