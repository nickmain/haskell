-- | Internal Constant Pool model
module JVM.ClassFile.ConstantPool.Model (

    CPoolIndex,
    CPEntry(..),
    ConstantPool,
    CPoolContainer(..),

    cpoolSize,
    cpoolEmpty,
    cpoolNullIndex,
    cpoolIsNullIndex,
    cpoolIndexOf,
    cpoolEntryAt,
    cpoolAddEntry,
    cpoolAppendEntry,
    cpoolSlotCount

) where {

    import qualified Data.Map as Map;
    import Data.Word;
    import Data.Int;
    import JVM.Types.Utils;
    import JVM.ClassFile.Util.JVMOp;
    import Control.Monad.State;
    import Control.Monad.Error;
    
    -- | Class for state types that contain a ConstantPool
    class CPoolContainer a where {
        getCP :: JVMOp a ConstantPool;    
        putCP :: ConstantPool -> JVMOp a ();
    };
    
    -- | Index of an entry in the constant pool
    type CPoolIndex = Word16;

    -- | ConstantPool ADT
    data ConstantPool = ConstantPool CPoolIndex {-next index-} PoolEntries PoolIndices
        deriving Eq;    
    
    type PoolEntries = Map.Map CPoolIndex CPEntry;
    type PoolIndices = Map.Map CPEntry CPoolIndex;

    -- | Get the slot count of the constant pool - where long and Double values
    --   occupy 2 slots.  This include the mythical slot zero.
    cpoolSize :: (CPoolContainer a) => JVMOp a Int;
    cpoolSize = do {
        (ConstantPool n _ _) <- getCP;
        return $ fromEnum (n - 1)
    };
    
    -- | An empty ConstantPool
    cpoolEmpty :: ConstantPool;
    cpoolEmpty = ConstantPool 1 Map.empty Map.empty;
    
    -- | The null index
    cpoolNullIndex :: CPoolIndex;
    cpoolNullIndex = 0;
    
    -- | Test whether an index is the null index
    cpoolIsNullIndex :: CPoolIndex -> Bool;
    cpoolIsNullIndex n = n == 0;
    
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

    
    -- | Look up the index of an entry - if there are duplicates then index of last is returned
    cpoolIndexOf :: (CPoolContainer a) => CPEntry -> JVMOp a CPoolIndex;
    cpoolIndexOf e = do {    
         (ConstantPool _ _ m) <- getCP;
         Map.lookup e m;
    };
    
    -- | Look up an entry by index - an unused slot will return Nothing
    cpoolEntryAt :: (CPoolContainer a) => CPoolIndex -> JVMOp a CPEntry;
    cpoolEntryAt i = do {
        (ConstantPool _ m _) <- getCP;
        Map.lookup i m;
    };
    
    -- | Add an entry to the pool (or find it if it already exists) 
    --   and return the index
    cpoolAddEntry :: (CPoolContainer a) => CPEntry -> JVMOp a CPoolIndex;
    cpoolAddEntry e = (cpoolIndexOf e) `catchError` (\_-> cpoolAppendEntry e);

    -- | Append an entry to the pool and return the index
    cpoolAppendEntry :: (CPoolContainer a) => CPEntry -> JVMOp a CPoolIndex;
    cpoolAppendEntry e = do {
        (ConstantPool cpi entries indices) <- getCP;
        
        putCP (ConstantPool ( cpi + (cpoolSlotCount e))
                            (Map.insert cpi e entries)
                            (Map.insert e cpi indices) );
        return cpi
    };


    -- Determine number of slots used by an entry
    cpoolSlotCount :: (Num a) => CPEntry -> a;
    cpoolSlotCount (CPLong   _) = 2;
    cpoolSlotCount (CPDouble _) = 2;
    cpoolSlotCount otherwise    = 1; 
            
    --------------------------------------------------------------------------
    -- Ord support follows..
    --------------------------------------------------------------------------
            
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
               
    --Compare two pairs of indices
    compareTwo :: (CPoolIndex,CPoolIndex) -> (CPoolIndex,CPoolIndex) -> Ordering;
    compareTwo (a,b) (c,d) | a == c = compare b d
                           | otherwise = compare a c;
    
    --Numbers used to order cp entries
    cpEntryOrd :: CPEntry -> Int;
    cpEntryOrd e = case e of {
        (CPClass       _  ) -> 1;
        (CPFieldRef    _ _) -> 2;
        (CPMethodRef _ _ _) -> 3;
        (CPString      _  ) -> 4;
        (CPInteger     _  ) -> 5;
        (CPFloat       _  ) -> 6;
        (CPLong        _  ) -> 7;
        (CPDouble      _  ) -> 8;
        (CPNameAndType _ _) -> 9;
        (CPUtf8        _  ) -> 10;
    };
}
