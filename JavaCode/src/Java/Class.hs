{- | Abstracted Representation of a Java Class.

	TODO: ANNOTATIONS
	

-}
module Java.Class where {

	-- | A Java class, interace, enum or annotation
	data Class = Class {
	                 className          :: String,        -- ^ fully qualified class name
	                 superName          :: Maybe String,  -- ^ superclass
					 classType          :: ClassType,       
					 classExtensibility :: Extensibility,   
					 isPublic           :: Bool,            
					 interfaces         :: [Type], 
					 fields             :: [Field],
					 staticFields       :: [Field],
	                 methods            :: [Method],
	                 staticmethods      :: [Method],
	                 sourceFile         :: Maybe String,    -- ^ source filename
	                 classAnnotations   :: [Annotation]
				 }
        deriving (Show,Eq);
			   
    -- | Extensibility of class, field or method
    data Extensibility = Extensible
                       | Final
                       | Abstract
        deriving (Show,Eq);
				
    -- | Type of class
	data ClassType = ClassClass
	               | InterfaceClass
	               | AnnotationClass
	               | EnumClass
        deriving (Show,Eq);
			   
	-- | A field member
	data Field = Field String             -- name 
	                   Type 
	                   Visibility 
	                   Bool               -- final
	                   [Annotation]	                   
	                   (Maybe Expression) -- initial or constant value
        deriving (Show,Eq);
	
	-- | A method member	
	data Method = Method Signature
	                     Visibility 
	                     Extensibility
	                     MethodQualifiers
	                     [Type]           -- exceptions
    	                 [Annotation]	                   
	                     (Maybe Statement) -- method code
        deriving (Show,Eq);

	-- | A method signature
	data Signature = Signature {
		methodName :: String, 
		returnType :: Type,
		argTypes   :: [Type]
    } deriving (Show,Eq);

	-- | Method qualifiers - (isFinal,isNative,isStrict)
	type MethodQualifiers = (Bool,Bool,Bool); 

	-- | Member visibility	                 
	data Visibility = Public | Private | Protected | Package
        deriving (Show,Eq);

	-- | An annotation
	
	                    
	-- | Code Context
	data CC = SourceLocation String Int Int -- ^ Filename Line Column
	        | CodeContext    String         -- ^ Context description
	        | NoContext                     -- ^ No applicable context
        deriving (Show,Eq);

	-- | Expressions
	data Expression = Constant      CC ConstantValue
	                | Call          CC CallType MethodRef [Expression]
	                | Assignment    CC Variable   Expression
	                | SetField      CC FieldRef   Expression
	                | FieldValue    CC FieldRef
	                | VariableValue CC Variable      
	                | NewObject     CC Type     (Maybe ConstructorCall)
	                | Conditional   CC Expression Expression Expression -- ^ a ? b : c
       deriving (Show,Eq);

	-- | Statements
	data Statement = Break      CC (Maybe Label)                          -- ^ Break out of a loop or switch
	               | Continue   CC (Maybe Label)                          -- ^ Continue a loop
	               | Loop       CC Expression {-head condition-} 
	                               Statement  {-body-} 
	                               Statement  {-between iterations-} 
	                               Expression {-tail condition-} 
	               | Return     CC (Maybe Expression)                     -- ^ Method return
	               | Switch     CC
	               | Throw      CC Expression
	               | Try        CC Statement [Catch] (Maybe Statement)    -- ^ Maybe Statement is the finally clause
	               | Branch     CC Label                                  -- ^ Unconditional branch
	               | If         CC Expression Statement (Maybe Statement) -- ^ Maybe is the else clause
	               | Evaluate   CC Expression                             -- ^ Evaluate an expression
		           | Label      CC Label
	               | Statements CC [Statement]
        deriving (Show,Eq);

	-- | A catch clause	
	data Catch = Catch {
	    exceptionType :: Type,
	    catchVariable :: Variable,
	    catchBlock    :: Statement
	} deriving (Show,Eq);
	
	-- | A statement label, target for branches, breaks and continues.
	type Label = String;
	
	-- | A variable - for now just the name
	type Variable = String;
	
	data Annotation = Annotation
		deriving (Show,Eq);	
		
    -- | Types
    data Type = TypeByte
    		  | TypeBoolean
    		  | TypeShort
    		  | TypeChar
    		  | TypeInt
    		  | TypeLong
    		  | TypeFloat
    		  | TypeDouble
    		  | TypeVoid
    		  | TypeObject String
		deriving (Show,Eq);	

	-- | Reference to a field
	data FieldRef = FieldRef {
		fieldClass    :: Type,
		fieldName     :: String,		
		fieldType     :: Type,
		fieldInstance :: Maybe Expression
	} deriving (Show,Eq);

	constructorName = "<init>";   -- ^ Method name for a constructor
	staticInitName  = "<clinit>"; -- ^ Method name for a static initializer

	-- | Types of instance call
	data CallType = Static | Virtual | Special | Interface
		deriving (Show,Eq);	
		
	-- | A Constructor call
	data ConstructorCall = ConstructorCall [Type] [Expression] -- ^ arg types & values
		deriving (Show,Eq);	
		
	-- | Reference to a method
	data MethodRef = MethodRef {
		methodClass  :: Type,
		methodSig    :: Signature,
		callInstance :: Maybe Expression
	} deriving (Show,Eq);

	-- | Constant Values
	data ConstantValue = ConstantInt    Integer
	                   | ConstantLong   Integer
	                   | ConstantFloat  Float
	                   | ConstantDouble Double
	                   | ConstantNull   
	                   | ConstantString String
	                   | ConstantClass  String
	    deriving (Show,Eq);
}
