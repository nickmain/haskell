{-# OPTIONS_GHC -fglasgow-exts #-}
-- | Representation of code in the Simple Language
module Epistem.SimpleLanguage.Code where {

    import Epistem.SimpleLanguage.Types;
    import Data.Tree;

    -- | Method or constructor code    
    data Code = Code [Statement];
    
    data Expression = Expression ExprType SourceLocation;
    
    -- | Expression types
    data ExprType   = Add         { firstArg, secondArg :: Expression }
                    | Subtract    { firstArg, secondArg :: Expression }
                    | Multiply    { firstArg, secondArg :: Expression }
                    | Divide      { firstArg, secondArg :: Expression }
                    | Remainder   { firstArg, secondArg :: Expression }
                    | ShiftLeft   { firstArg, secondArg :: Expression }
                    | ShiftRight  { firstArg, secondArg :: Expression }
                    | ShiftRightU { firstArg, secondArg :: Expression }
                    | Compare     { compareOp :: Op, firstArg, secondArg :: Expression }
                    | ArrayLength { firstArg :: Expression }
                    | Negate      { firstArg :: Expression }
                    | And         { firstArg, secondArg :: Expression }
                    | Or          { firstArg, secondArg :: Expression }
                    | Xor         { firstArg, secondArg :: Expression }
                    | InstanceOf  { typeArg :: Type, firstArg :: Expression }
                    | Cast        { typeArg :: Type, firstArg :: Expression }
                    | Convert     { typeArg :: Type, firstArg :: Expression }
                    | CallSpecial { methodRef :: MethodRef, instanceArg :: Expression, methodArgs :: [Expression] }
                    | CallVirtual { methodRef :: MethodRef, instanceArg :: Expression, methodArgs :: [Expression] }
                    | CallStatic  { methodRef :: MethodRef, methodArgs :: [Expression] }
                    | ReadField   { fieldRef :: FieldRef, instanceArg :: Expression }
                    | ReadStatic  { fieldRef :: FieldRef }  
                    | ReadElement { arrayArg, indexArg :: Expression }
                    | Conditional { conditionArg, firstArg, secondArg :: Expression }
                    | Assignment  { assignTarget :: AssignmentTarget, firstArg :: Expression } 
                    | Constant    { constValue :: Constant }
                    | New         { constructorRef :: ConstructorRef, methodArgs :: [Expression] }
                    | NewArray    { elemType :: Type, dimensionSizes :: [Expression], arrayInitializer :: (Maybe [Expression]) }
                    ;

    -- | Info about location within source
    data SourceLocation = SrcLoc String {-file-} Int {-line-} Int {-col-};

    -- | Comparison operations
    data Op = Equal 
            | NotEqual 
            | LessThan 
            | LessOrEqual 
            | GreaterThan 
            | GreateOrEqual
            ;

    -- | Things that can be assigned to
    data AssignmentTarget = ElementTarget  Expression Expression
                          | VariableTarget String
                          | FieldTarget    FieldRef Expression
                          | StaticField    FieldRef
                          ;

    -- | Reference to a method
    data MethodRef = MethodRef String     -- Class name
                               String     -- Method name
                               [Type]     -- Parameter types
                               Type       -- Return type
                   ;
                   
    -- | Reference to a field
    data FieldRef = FieldRef String     -- Class name
                             String     -- Field name
                             Type       -- Field type
                  ;
                   
    -- | Reference to a constructor
    data ConstructorRef = ConstructorRef String -- Class Name
                                         [Type] -- Parameter types 
                        ;
                        
    data Statement = Statement StmtType SourceLocation;
                                                        
    -- | The statement types                    
    data StmtType = Evaluate  { expression :: Expression }
                  | Return    { retValue :: Maybe Expression }
                  | If        { condition :: Expression, body :: Statement }
                  | IfElse    { condition :: Expression, body, elseBody :: Statement }
                  | Try       { body :: Statement, catches :: [Catch], finally :: Finally }
                  | Throw     { expression :: Expression }
                  | Sync      { expression :: Expression, body :: Statement }
                  | WhileLoop { condition  :: Expression, body :: Statement, label :: Maybe Label }
                  | DoLoop    { body :: Statement, condition :: Expression, label :: Maybe Label }
                  | ForLoop   { condition :: Expression, body :: Statement, loopAction :: Statement, label :: Maybe Label }
                  | Break     { maybeLabel :: (Maybe Label) }
                  | Continue  { maybeLabel :: (Maybe Label) }
                  | Block     { blockBody :: [Statement] }
                  | Switch    { cases :: [Case] }
                  ;
                   
    type Label = String;

    data Case = Case [Integer]
                     Bool          -- True for default case
                     Statement;

    data Catch = Catch String   -- exception class name
                       Statement;

    type Finally = Maybe Statement;

}
