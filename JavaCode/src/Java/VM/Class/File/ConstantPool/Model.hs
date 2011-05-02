-- | Internal Constant Pool model
module Java.VM.Class.File.ConstantPool.Model where {

    import qualified Data.Map as Map;
    import Data.Word;
    import Data.Int;
    import Java.Types.Utils;
    
    -- | Index of an entry in the constant pool
    newtype CPoolIndex = CPoolIndex Word16 
        deriving Show;

    -- | ConstantPool ADT
    data ConstantPool = ConstantPool CPoolIndex {-next index-} PoolEntries PoolIndices
        deriving Eq;    
    
    type PoolEntries = Map.Map CPoolIndex CPEntry;
    type PoolIndices = Map.Map CPEntry CPoolIndex;

    -- | Get the slot count of the constant pool - where long and Double values
    --   occupy 2 slots.  This include the mythical slot zero.
    cpoolSize :: ConstantPool -> Int;
    cpoolSize (ConstantPool (CPoolIndex n) _ _) = fromEnum (n - 1);
    
    -- | An empty ConstantPool
    cpoolEmpty :: ConstantPool;
    cpoolEmpty = ConstantPool (CPoolIndex 1) Map.empty Map.empty;
    
    -- | The null index
    cpoolNullIndex :: CPoolIndex;
    cpoolNullIndex = CPoolIndex 0;
    
    -- | Test whether an index is the null index
    cpoolIsNullIndex :: CPoolIndex -> Bool;
    cpoolIsNullIndex (CPoolIndex n) = n == 0;
    
    -- | A field reference: (class-name,field-name,field-type).
    type FieldRef = (JavaClassName,String,JavaType);

    -- | A method reference: (class-name,method-name,signature).
    type MethodRef = (JavaClassName,String,Signature);

    -- | A name and type: (name,type-string).
    type NameAndType = (String,String); 

    -- | The possible constant values
    data ConstantEntry = ConstInt    Int32
                       | ConstFloat  Float
                       | ConstLong   Int64
                       | ConstDouble Double
                       | ConstString String
                       | ConstClass  JavaClassName;
    

    instance Show ConstantPool where {    
        show (ConstantPool _ m _) = 
            "ConstantPool:\n" ++ 
            (concat $ map (\(idx,entry)->"  " ++ (show idx) ++ ": " ++ (show entry) ++ "\n") 
                    $ Map.toList m);
    };
    
        -- An entry in the constant pool
    data CPEntry = CPClass       CPoolIndex            -- ^ utf8 name index
                 | CPFieldRef    CPoolIndex CPoolIndex -- ^ class index, name+type index
                 | CPMethodRef   CPoolIndex CPoolIndex Bool -- ^ class index, name+type index, True for interface method
                 | CPString      CPoolIndex            -- ^ utf8 index
                 | CPInteger     Int32
                 | CPFloat       Float
                 | CPLong        Int64
                 | CPDouble      Double
                 | CPNameAndType CPoolIndex CPoolIndex -- ^ utf8 name index, utf8 desc index
                 | CPUtf8        String
        deriving (Show,Eq);

    
    -- Look up the index of an entry - if there are duplicates then index of last is returned
    cpoolIndexOf :: (Monad m) => ConstantPool -> CPEntry -> m CPoolIndex;
    cpoolIndexOf (ConstantPool _ _ m) e = Map.lookup e m; 
    
    -- Look up an entry by index - an unused slot will return Nothing
    cpoolEntryAt :: (Monad m) => ConstantPool -> CPoolIndex -> m CPEntry;
    cpoolEntryAt (ConstantPool _ m _) i = Map.lookup i m;
        
    -- Add an entry to the pool (or find it if it already exists) 
    -- and return the index of the entry and the updated pool
    cpoolAddEntry :: ConstantPool -> CPEntry -> (CPoolIndex,ConstantPool);
    cpoolAddEntry cp e = case cpoolIndexOf cp e of {
        (Just idx) -> (idx,cp);
        Nothing    -> cpoolAppendEntry cp e;
    };

    -- Add an entry to the pool and return the index of the entry and the updated pool
    cpoolAppendEntry :: ConstantPool -> CPEntry -> (CPoolIndex,ConstantPool);
    cpoolAppendEntry (ConstantPool cpi@(CPoolIndex n) entries indices) e = 
        ( cpi, ConstantPool (CPoolIndex( n + (cpoolSlotCount e)))
                            (Map.insert cpi e entries)
                            (Map.insert e cpi indices) );


    -- Determine number of slots used by an entry
    cpoolSlotCount :: CPEntry -> Word16;
    cpoolSlotCount (CPLong   _) = 2;
    cpoolSlotCount (CPDouble _) = 2;
    cpoolSlotCount otherwise    = 1; 
            
    instance Ord CPEntry where {
        compare (CPClass       a  ) (CPClass       b  ) = compare a b;
        compare (CPFieldRef    a b) (CPFieldRef    c d) = compareTwo (a,b) (c,d);
        compare (CPMethodRef a b _) (CPMethodRef c d _) = compareTwo (a,b) (c,d);
        compare (CPString      a  ) (CPString      b  ) = compare a b;
        compare (CPInteger     a  ) (CPInteger     b  ) = compare a b;
        compare (CPFloat       a  ) (CPFloat       b  ) = compare a b;
        compare (CPLong        a  ) (CPLong        b  ) = compare a b;
        compare (CPDouble      a  ) (CPDouble      b  ) = compare a b;
        compare (CPNameAndType a b) (CPNameAndType c d) = compareTwo (a,b) (c,d);
        compare (CPUtf8        a  ) (CPUtf8        b  ) = compare a b;
        
        compare a b = compare (cpEntryOrd a) (cpEntryOrd b);
    };

    instance Eq CPoolIndex where {
        (CPoolIndex a) == (CPoolIndex b) = a == b;    
    };
    
    instance Ord CPoolIndex where {
        compare (CPoolIndex a) (CPoolIndex b) = compare a b;    
    };
               
    --Compare two pairs of indices
    compareTwo :: (CPoolIndex,CPoolIndex) -> (CPoolIndex,CPoolIndex) -> Ordering;
    compareTwo (a,b) (c,d) | a == c = compare b d
                           | otherwise = compare a c;
    
    --Numbers used to order cp entries
    cpEntryOrd :: CPEntry -> Int;
    cpEntryOrd (CPClass       _  ) = 1;
    cpEntryOrd (CPFieldRef    _ _) = 2;
    cpEntryOrd (CPMethodRef _ _ _) = 3;
    cpEntryOrd (CPString      _  ) = 4;
    cpEntryOrd (CPInteger     _  ) = 5;
    cpEntryOrd (CPFloat       _  ) = 6;
    cpEntryOrd (CPLong        _  ) = 7;
    cpEntryOrd (CPDouble      _  ) = 8;
    cpEntryOrd (CPNameAndType _ _) = 9;
    cpEntryOrd (CPUtf8        _  ) = 10;
}
