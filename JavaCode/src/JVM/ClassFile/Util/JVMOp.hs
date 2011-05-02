-- | Error and State monad for JVM monadic operations
module JVM.ClassFile.Util.JVMOp where {

    import Control.Monad.State;
    import Control.Monad.Error;
    
    -- | The state type
    data JVMState a = JVMState a;
    
    -- | The state monad
    type JVMStateMonad a = State (JVMState a);
    
    -- | The error/state monad stack
    type JVMOp a = ErrorT JVMException (JVMStateMonad a);
    
    -- | Type for JVMOp exceptions
    type JVMException = String;
    
    -- | Get the inner state
    getState :: JVMOp a a;
    getState = do {
        (JVMState a) <- get;
        return a
    };
    
    -- | Put the inner state
    putState :: a -> JVMOp a ();
    putState a = do {
        put (JVMState a)
    };
}
